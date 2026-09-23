package okay.scala2

import okay.{!, Chunk, Timer}
import okay.given
import okay.actor.{Actor, ActorRef, Reply, Supervise}
import okay.http.Request
import okay.kafka.KafkaInterop
import okay.obs.{Attr, Log, Tracer}
import okay.ops.{Lifecycle, Ops, Red}
import okay.outbox.{Inbox, Outbox}
import okay.persist.Store
import org.apache.kafka.clients.consumer.{Consumer, ConsumerRecord}
import org.apache.kafka.clients.producer.{Producer, ProducerRecord}

/*
 * okay-actor, okay-outbox, okay-obs, okay-ops, okay-kafka and okay-pg for
 * Scala 2.13 (specs/scala2-facade.md, stage 15.8).
 *
 * Probed first. The values and the builders are plain and Scala 2 uses
 * them directly: `ActorRef`, `Supervise`, `Reply`, `new Outbox()`,
 * `new Inbox()`, `Dialect`, `Log.Line`/`Log.Level`, `new Tracer(topic)`
 * and its `span(name)(body)`, `new Red(name)`, `new Lifecycle()`,
 * `Prom.render`, `new KafkaStore(bootstrap)` (an okay-persist `Store`,
 * so section 8k's `Persist` works over Kafka as it is). What does not
 * carry over is every OPERATION that answers a program, and those are
 * the objects below, each named apart from the library's own.
 */

/** okay-actor's operations */
object Actors {
  /** an actor: its state, and what a message does to it. A behaviour
   * that throws stops the actor. */
  def spawn[S, M](init: S)(behavior: (S, M) => Eff[Async, S]): Eff[Async, ActorRef[M]] =
    Async.lift(Actor.spawn[S, M](init)((s, m) => Async.core(behavior(s, m))))

  /** an actor with its own mailbox capacity and a supervision policy
   * (`Supervise.Stop`, `Resume`, `Restart(() => fresh)`, `Escalate(f)`) */
  def spawn[S, M](init: S, supervise: Supervise[S], capacity: Int)(behavior: (S, M) => Eff[Async, S]): Eff[Async, ActorRef[M]] =
    Async.lift(Actor.spawn[S, M](init, okay.Channel[M](capacity), supervise)((s, m) => Async.core(behavior(s, m))))

  /** an actor stopped with its parent: `stop(parent)` stops it first */
  def child[S, M](parent: ActorRef[?], init: S, supervise: Supervise[S] = Supervise.Stop)
                 (behavior: (S, M) => Eff[Async, S]): Eff[Async, ActorRef[M]] =
    Async.lift(parent.spawnChild[S, M](init, okay.Channel[M](256), supervise)((s, m) => Async.core(behavior(s, m))))

  /** fire and forget; false if the actor has stopped */
  def tell[M](actor: ActorRef[M], m: M): Eff[Async, Boolean] = Async.lift(actor.tell(m))

  /** send a message carrying a `Reply` box, and wait up to `within`
   * milliseconds for the answer; None if it did not come */
  def ask[M, R](actor: ActorRef[M], within: Long)(message: Reply[R] => M): Eff[Async, Option[R]] =
    Async.lift(actor.ask[R](message, within))

  /** stop, handling every message already accepted first */
  def stop(actor: ActorRef[?]): Eff[Async, Unit] = Async.lift(actor.stop())
}

/** okay-outbox's operations, over okay-scala2-sql's `Db` */
object Outboxes {
  /** write a message in the same database (and so the same transaction)
   * as the business change; the relay publishes it later. The answer is
   * the message's id. */
  def enqueue(outbox: Outbox, db: Db, topic: String, value: Array[Byte],
              key: Array[Byte] = Array.empty, part: Int = 0): Eff[Async, String] =
    Async.lift(outbox.enqueue(db.underlying, topic, value, key, part))

  /** publish up to `batch` unpublished messages to `store`; how many */
  def relayOnce(outbox: Outbox, db: Db, store: Store, batch: Int = 256): Eff[Async, Int] =
    Async.lift(outbox.relayOnce(db.underlying, store, batch))

  /** how many messages wait to be published */
  def pending(outbox: Outbox, db: Db): Eff[Async, Long] = Async.lift(outbox.pending(db.underlying))

  /** true the first time `id` is seen, false every time after */
  def first(inbox: Inbox, db: Db, id: String): Eff[Async, Boolean] = Async.lift(inbox.first(db.underlying, id))

  /** run `body` once per message id, in one transaction with the
   * record that it ran; None for a message already handled */
  def once[A](inbox: Inbox, db: Db, id: String)(body: Eff[Async, A]): Eff[Async, Option[A]] =
    Async.lift(inbox.once[A](db.underlying, id)(Async.core(body)))
}

/** okay-obs' log lines, as a Writer of `Log.Line` */
object Logs {
  def debug(message: String, fields: (String, String)*): Eff[Writer[Log.Line], Unit] = at(Log.Level.Debug, message, fields)
  def info(message: String, fields: (String, String)*): Eff[Writer[Log.Line], Unit] = at(Log.Level.Info, message, fields)
  def warn(message: String, fields: (String, String)*): Eff[Writer[Log.Line], Unit] = at(Log.Level.Warn, message, fields)
  def error(message: String, fields: (String, String)*): Eff[Writer[Log.Line], Unit] = at(Log.Level.Error, message, fields)

  /** an error with the throwable's class and message as fields */
  def failure(message: String, e: Throwable, fields: (String, String)*): Eff[Writer[Log.Line], Unit] =
    at(Log.Level.Error, message, fields ++ Seq("error" -> e.getClass.getName, "error.message" -> Option(e.getMessage).getOrElse("")))

  /** send each line at `min` or above to `write` as it is logged, stamped
   * with `clock` (lines below `min` are dropped) */
  def to[R, A](write: Log.Line => Unit, min: Log.Level = Log.Level.Info, clock: () => Long = () => System.currentTimeMillis)
              (e: Eff[Writer[Log.Line] & R, A]): Eff[R, A] = {
    val sink: okay.Fold[Log.Line, Unit] = okay.Fold(())((_, l) =>
      if (l.level.atLeast(min)) write(if (l.at == 0L) l.copy(at = clock()) else l))
    Eff.of(Rows.coerce(okay.Writer.fold[Log.Line, Unit, A, Rows.Top](Rows.coerce(e.program))(using summon, sink).map(_._2)))
  }

  private def at(level: Log.Level, message: String, fields: Seq[(String, String)]): Eff[Writer[Log.Line], Unit] =
    Writer.tell(Log.Line(level, message, fields.toVector.map { case (k, v) => Attr(k, v) }))
}

/** okay-obs' spans around a program */
object Tracing {
  /** run `e` inside a span named `name`, a child of the current one */
  def span[A](tracer: Tracer, name: String, attrs: (String, String)*)(e: Eff[Async, A]): Eff[Async, A] =
    Async.delay(tracer.span(name, attrs.map { case (k, v) => Attr(k, v) }*)(Eff.runAsync(e)))
}

/** okay-ops' endpoints and meters, in okay-scala2-http's terms */
object Operations {
  /** GET /healthz, /readyz, /metrics (Prometheus) and /stats over `store`,
   * with the lifecycle and RED meters if given */
  def routes(store: Store, lifecycle: Option[Lifecycle] = None, red: Seq[Red] = Nil): PartialFunction[Request, Eff[Async, Response]] =
    fromCore(Ops.routes(store, lifecycle = lifecycle, red = red.toVector))

  /** count requests, errors and durations per `label`, around `routes` */
  def measured(red: Red, label: Request => String)(routes: PartialFunction[Request, Eff[Async, Response]]): PartialFunction[Request, Eff[Async, Response]] =
    fromCore(red.route(label)(toCore(routes)))

  /** refuse new requests once draining has begun, and count those in flight */
  def admitted(lifecycle: Lifecycle)(routes: PartialFunction[Request, Eff[Async, Response]]): PartialFunction[Request, Eff[Async, Response]] =
    fromCore(lifecycle.route(toCore(routes)))

  /** stop admitting, wait up to `graceMillis` for the requests in flight;
   * true if they all finished */
  def drain(lifecycle: Lifecycle, graceMillis: Long): Eff[Async, Boolean] =
    Async.lift(lifecycle.drain(graceMillis)(using summon[Timer]))

  private def toCore(pf: PartialFunction[Request, Eff[Async, Response]]): PartialFunction[Request, okay.http.Response ! okay.Async] = {
    case r if pf.isDefinedAt(r) => Async.core(pf(r).map(_.core))
  }

  private def fromCore(pf: PartialFunction[Request, okay.http.Response ! okay.Async]): PartialFunction[Request, Eff[Async, Response]] = {
    case r if pf.isDefinedAt(r) => Async.lift(pf(r)).flatMap(Response.read)
  }
}

/** okay-kafka's program-shaped operations; `KafkaStore` needs none */
object Kafkas {
  /** the consumer's records as they are polled */
  def source[K, V](consumer: Consumer[K, V], pollMillis: Long = 1000): Source[ConsumerRecord[K, V]] =
    Source.of(okay.Writer.expand[Chunk[ConsumerRecord[K, V]], ConsumerRecord[K, V], Unit, okay.Async](
      KafkaInterop.source(consumer, pollMillis))(c => c))

  def commit[K, V](consumer: Consumer[K, V]): Eff[Async, Unit] = Async.lift(KafkaInterop.commit(consumer))

  def send[K, V](producer: Producer[K, V], records: Seq[ProducerRecord[K, V]]): Eff[Async, Unit] =
    Async.lift(KafkaInterop.sink(producer)(scala.collection.immutable.ArraySeq.from(records)))
}

/** okay-pg: a `Db` (okay-scala2-sql) over Postgres' own wire protocol */
object Postgres {
  def connect(host: String, port: Int, user: String, password: String, database: String): Eff[Async, Db] = {
    import okay.crypto.platform // SCRAM's digests: the platform's own
    Async.lift(okay.pg.PgSql.connect(host, port, user, password, database).map(Db(_)))
  }
}
