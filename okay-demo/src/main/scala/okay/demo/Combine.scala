package okay.demo

import okay.*
import okay.given
import okay.!.{Bind, Effect, Pure, resume, widen}

import java.time.Instant

/**
 * The stream-joining exercise, in okay: two live sources — charging
 * telemetry and battery telemetry — merged by READINESS, and every
 * charging reading enriched with the last state of charge seen for
 * that vehicle. (The fs2 original is ../exercise/stream-exercise:
 * `battery.map(Left).merge(charging.map(Right)).mapAccumulate(repo)`.)
 *
 * It is written in the two halves the library separates, and that is
 * the whole point of having it here:
 *
 *   - `combine` is a PURE Stage — awaits events, tells outputs, and
 *     its state is a recursion parameter, not a cell and not a
 *     `mapAccumulate`. It has no effects at all, so it is testable
 *     with `pipe` over a list: no scheduler, no clock, no waiting.
 *   - `outputs` is the concurrent half: `Channel.merge` runs a fiber
 *     per source and interleaves them by whoever is ready, and the
 *     SAME stage is then run over the merged stream by `through`.
 *
 * Nothing between them is materialized: the consumer's demand pulls
 * one event through the merge at a time.
 */
object Combine {

  // the wrappers of the exercise, for correctness at the call site
  final case class Timestamp(value: Instant)
  final case class VehicleId(value: String)
  final case class SocketId(value: String)
  final case class StateOfChargeInPercent(value: Int)
  final case class PowerInWatts(value: Int)

  final case class Battery(timestamp: Timestamp,
                           vehicleId: VehicleId,
                           stateOfChargeInPercent: StateOfChargeInPercent)

  final case class Charging(timestamp: Timestamp,
                            socketId: SocketId,
                            vehicleId: VehicleId,
                            powerInWatts: PowerInWatts)

  final case class Output(timestamp: Timestamp,
                          socketId: SocketId,
                          vehicleId: VehicleId,
                          powerInWatts: PowerInWatts,
                          stateOfChargeInPercent: StateOfChargeInPercent)

  /**
   * What the merged stream carries: a UNION, where the fs2 version
   * needs an `Either` — the effect rows are unions already, and a
   * union costs no wrapper and no allocation per element. The stage
   * below splits it by an ordinary type test.
   */
  type Event = Battery | Charging

  /** the last state of charge per vehicle (the exercise's interface,
   * unchanged: persistent, so the stage stays pure) */
  trait StateRepo:
    def get(vehicleId: VehicleId): Option[StateOfChargeInPercent]
    def put(vehicleId: VehicleId, value: StateOfChargeInPercent): StateRepo

  /** an asynchronous source: a program that TELLS its elements as it
   * goes, performing Async between them (okay-llm's shape exactly) */
  type Source[W] = Unit ! (Writer % W + Async)

  /**
   * A source IS a stream of what it tells — Writer's own observation
   * with the answer forgotten. This is the instance `Channel.merge`
   * asks for; the library gives it only for the PURE writer, because
   * inference cannot reach through the type lambda by itself.
   */
  given Stream[Source, Async] with
    def uncons[W](s: Source[W]): Option[(W, Source[W])] ! Async =
      Writer.uncons[W, Unit, Async](s).map(_.toOption)

  /**
   * The join itself: await an event, tell an output when one is due,
   * answer the final repository.
   *
   * Battery readings only update the state; a charging reading emits
   * — and emits NOTHING when that vehicle has no state of charge yet,
   * which is the exercise's "only emit when charging data arrives"
   * plus "the last received state of charge". The state is the
   * recursion's parameter, so there is nothing to reset between runs
   * and nothing to hide a mutation in.
   */
  def combine(repo: StateRepo): Stage[Event, Output, StateRepo] =
    Stage.await[Event, Output].flatMap {
      case None => pure(repo)
      case Some(b: Battery) =>
        combine(repo.put(b.vehicleId, b.stateOfChargeInPercent))
      case Some(c: Charging) => repo.get(c.vehicleId) match
        case None => combine(repo)
        case Some(soc) =>
          Stage.tell[Event, Output](Output(
            timestamp = c.timestamp,
            socketId = c.socketId,
            vehicleId = c.vehicleId,
            powerInWatts = c.powerInWatts,
            stateOfChargeInPercent = soc)).flatMap(_ => combine(repo))
    }

  /**
   * The concurrent half: the two sources merged by READINESS (a fiber
   * per source feeding one channel — the loser of every race simply
   * arrives later), then the pure stage run over the merge. Lazy
   * throughout: the fold at the end is what pulls.
   */
  def outputs(repo: StateRepo, battery: Source[Battery], charging: Source[Charging])
             (using Scheduler, CanBlock): (Seq[Output], StateRepo) =
    Writer.run[Output, StateRepo, Async](joined(repo, battery, charging)).runWith

  /** the joined stream as a program: outputs told, Async performed at
   * every pull, nothing run until something folds it */
  def joined(repo: StateRepo, battery: Source[Battery], charging: Source[Charging])
            (using Scheduler, CanBlock): StateRepo ! (Writer % Output + Async) =
    val merged = Channel.merge[Event, Source, Async, Source, Async](
      tag(battery)(identity), tag(charging)(identity))
    through[Event, Output, Async, Unit, StateRepo](source(merged))(
      // the stage is pure; `through` composes in the wider row, and
      // Free is invariant in its signature, so the embedding is a walk
      widen[StateRepo, Take % Event + Writer % Output, Async](combine(repo)))

  /**
   * Re-tell a source's elements at another type: `Writer` is
   * invariant in what it tells, so a `Source[Battery]` is not a
   * `Source[Event]` for free — this is the one element-wise walk, and
   * it is where the two sources become one type. Async operations
   * met on the way are forwarded untouched, in order.
   */
  def tag[W, V](s: Source[W])(f: W => V): Source[V] =
    (s.resume: @unchecked) match
      case Pure(_) => pure(())
      case Effect(e) => <|>[Async, Writer % W](e) match
        case Left(a) => Effect(a).map(_ => ())
        case Right(Writer.Say(w)) => emit(f(w))
      case Bind(Effect(e), k) => <|>[Async, Writer % W](e) match
        case Left(a) => Effect(a).flatMap(x => tag(k(x))(f))
        case Right(Writer.Say(w)) => emit(f(w)).flatMap(_ => tag(k(()))(f))

  /** a channel as a source: each pull receives one element (parking a
   * virtual thread if there is nothing yet), the close ends it */
  def source[A](c: Channel[A]): Source[A] =
    effect[Writer % A + Async, Option[A]](Async.Run(() => c.receive())).flatMap {
      case Some(a) => emit(a).flatMap(_ => source(c))
      case None => pure(())
    }

  /** tell one element into a source's row */
  inline def emit[W](w: W): Source[W] = effect(Writer(w))

  /** every element of a sequence, told in order */
  def emitAll[W](ws: Seq[W]): Source[W] =
    ws.foldRight(pure[Writer % W + Async, Unit](()))((w, rest) =>
      emit(w).flatMap(_ => rest))
}
