package okay.npm

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import okay.{!, Async, Channel, effect, pure}
import okay.codec.{Json, Schema, Stubs}
import okay.crdt.{GCounter, NodeId, OrSet, PNCounter}
import okay.crdt.Wire.given
import okay.ts.{Journal, Ts}

/**
 * okay for TypeScript projects, as an npm package (typescript-types T9).
 * Every `@JSExportTopLevel` here is an export of the ES module, and
 * `declarations` is the module's `index.d.ts`, written from the same
 * Schemas that encode its values, so the package's types and its data
 * cannot drift.
 *
 *  - `run(program, callbacks)`: a TypeScript program built with `done`,
 *    `perform` and `then`, walked by okay; each named operation is a
 *    callback of the caller's, synchronous or a Promise;
 *  - `gcounter`, `pncounter`, `orset`: CRDT replicas as plain JSON
 *    states, each with its `merge` (commutative, associative, idempotent —
 *    okay-crdt's laws, tested there);
 *  - `channel()`: an okay `Channel` a TypeScript consumer reads with
 *    `for await`.
 */
object Okay:

  given scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

  private def refuse(what: String, why: String): Nothing = throw js.JavaScriptException(js.TypeError(s"$what: $why"))

  private def read[A](what: String, v: js.Any)(using Schema[A]): A =
    Ts.fromJs[A](v).fold(f => refuse(what, f.message), identity)

  private def plain(j: Json): js.Any = js.JSON.parse(Json.print(j))

  private def asJson(v: js.Any): Json = (js.JSON.stringify(v): Any) match
    case s: String => Json.parse(s)
    case _ => Json.JNull

  // ---------------------------------------------------------------- programs

  @JSExportTopLevel("done")
  val done: js.Function1[js.Any, js.Any] = (value: js.Any) => js.Dynamic.literal(tag = "done", value = value)

  @JSExportTopLevel("perform")
  val perform: js.Function = js.Any.fromFunction2 { (name: String, args: js.Array[js.Any]) =>
    js.Dynamic.literal(tag = "perform", name = name, args = args, k = done)
  }

  /** `perform(name, ...args)`: the variadic spelling TypeScript writes */
  @JSExportTopLevel("performing")
  def performing(name: String, args: js.Any*): js.Any =
    js.Dynamic.literal(tag = "perform", name = name, args = args.toJSArray, k = done)

  @JSExportTopLevel("then")
  def andThen(p: js.Dynamic, f: js.Function1[js.Any, js.Any]): js.Any =
    if (p.selectDynamic("tag"): Any) == "done" then f(p.selectDynamic("value"))
    else
      val k = p.selectDynamic("k")
      js.Dynamic.literal(tag = "perform", name = p.selectDynamic("name"), args = p.selectDynamic("args"),
        k = (x: js.Any) => andThen(js.Dynamic.global.Object(k(x)), f))

  /** a callback's answer — a value, or a Promise of one — as an okay
   * step. `Promise.resolve` takes either, so there is one road and no
   * type test on what the callback chose to return */
  private def settle(v: js.Any): Either[Ts.Failure, Json] ! Async =
    effect[Async, Either[Ts.Failure, Json]](Async.Await[Either[Ts.Failure, Json]] { k =>
      js.Promise.resolve[js.Any](v).toFuture.onComplete {
        case scala.util.Success(x) => k(Right(Right(asJson(x))))
        case scala.util.Failure(js.JavaScriptException(e)) => k(Right(Left(failure(e))))
        case scala.util.Failure(other) => k(Right(Left(Ts.Failure("Error", String.valueOf(other.getMessage)))))
      }
      () => ()
    })

  private def failure(e: Any): Ts.Failure = e match
    case err: js.Error => Ts.Failure(err.name, err.message)
    case other => Ts.Failure("Error", String.valueOf(other))

  @JSExportTopLevel("run")
  def run(program: js.Any, callbacks: js.Dictionary[js.Function]): js.Promise[js.Any] =
    answer(Ts.runJson[Async](program, jsCallbacks(callbacks)))

  private def answer(p: Either[Ts.Failure, Json] ! Async): js.Promise[js.Any] =
    Async.runAsync(p).flatMap {
      case Right(v) => scala.concurrent.Future.successful(plain(v))
      case Left(f) => scala.concurrent.Future.failed(js.JavaScriptException(js.Error(s"${f.kind}: ${f.message}")))
    }.toJSPromise

  private def jsCallbacks(callbacks: js.Dictionary[js.Function]): Ts.Callbacks[Async] =
    Ts.callbacks[Async](callbacks.toVector.map { (name, f) =>
      Ts.Callback[Async](name, args =>
        val in = args match
          case Json.JArr(xs) => xs.map(plain)
          case one => Vector(plain(one))
        try settle(f.call(js.undefined, in*))
        catch case js.JavaScriptException(e) => pure(Left(failure(e))))
    }*)

  // ---------------------------------------------------------- durable flows

  /** a Promise the caller handed over, as an okay step */
  private def awaiting(p: js.Any): js.Any ! Async =
    effect[Async, js.Any](Async.Await[js.Any] { k =>
      js.Promise.resolve[js.Any](p).toFuture.onComplete(t => k(t.toEither))
      () => ()
    })

  /** okay's Journal as the JS object the package declares: entries as JSON text */
  private def exported(j: Journal): js.Object & js.Dynamic = js.Dynamic.literal(
    load = (flow: String) => Async.runAsync(j.load(flow)).map(_.map(Json.print).toJSArray).toJSPromise,
    append = (flow: String, step: Double, entry: String) =>
      Async.runAsync(j.append(flow, step.toInt, Json.parse(entry))).toJSPromise,
    clear = (flow: String) => Async.runAsync(j.clear(flow)).toJSPromise,
  )

  /** any object with the declared `Journal` shape — okay's own two, or one
   * of the caller's (localStorage, a server) — as okay's Journal */
  private def imported(o: js.Dynamic): Journal = new Journal:
    def load(flow: String): Vector[Json] ! Async =
      awaiting(o.load(flow)).map(v => asJson(v) match
        case Json.JArr(texts) => texts.collect { case Json.JStr(t) => Json.parse(t) }
        case _ => Vector.empty)
    def append(flow: String, step: Int, entry: Json): Unit ! Async =
      awaiting(o.append(flow, step, Json.print(entry))).map(_ => ())
    def clear(flow: String): Unit ! Async = awaiting(o.clear(flow)).map(_ => ())

  @JSExportTopLevel("memoryJournal")
  def memoryJournal(): js.Object & js.Dynamic = exported(Journal.memory())

  @JSExportTopLevel("indexedDbJournal")
  def indexedDbJournal(name: String): js.Object & js.Dynamic = exported(Journal.indexedDb(name))

  /** `run`, with every answer journalled first: a reload resumes the flow */
  @JSExportTopLevel("durable")
  def durable(flow: String, program: js.Any, callbacks: js.Dictionary[js.Function], journal: js.Dynamic): js.Promise[js.Any] =
    answer(Ts.durableJson(flow, program, jsCallbacks(callbacks), imported(journal)))

  // ------------------------------------------------------------------- CRDTs

  private def node(n: String): NodeId = NodeId(n)
  private def by(n: js.UndefOr[Double]): Long = n.fold(1L)(_.toLong)

  @JSExportTopLevel("gcounter")
  val gcounter: js.Object & js.Dynamic = js.Dynamic.literal(
    empty = () => Ts.toJs(GCounter.empty),
    inc = (c: js.Any, n: String, k: js.UndefOr[Double]) => Ts.toJs(read[GCounter]("gcounter.inc", c).inc(node(n), by(k))),
    merge = (a: js.Any, b: js.Any) =>
      Ts.toJs(okay.crdt.Crdt[GCounter].merge(read[GCounter]("gcounter.merge", a), read[GCounter]("gcounter.merge", b))),
    value = (c: js.Any) => read[GCounter]("gcounter.value", c).value.toDouble,
  )

  @JSExportTopLevel("pncounter")
  val pncounter: js.Object & js.Dynamic = js.Dynamic.literal(
    empty = () => Ts.toJs(PNCounter.empty),
    inc = (c: js.Any, n: String, k: js.UndefOr[Double]) => Ts.toJs(read[PNCounter]("pncounter.inc", c).inc(node(n), by(k))),
    dec = (c: js.Any, n: String, k: js.UndefOr[Double]) => Ts.toJs(read[PNCounter]("pncounter.dec", c).dec(node(n), by(k))),
    merge = (a: js.Any, b: js.Any) =>
      Ts.toJs(okay.crdt.Crdt[PNCounter].merge(read[PNCounter]("pncounter.merge", a), read[PNCounter]("pncounter.merge", b))),
    value = (c: js.Any) => read[PNCounter]("pncounter.value", c).value.toDouble,
  )

  @JSExportTopLevel("orset")
  val orset: js.Object & js.Dynamic = js.Dynamic.literal(
    empty = () => Ts.toJs(OrSet.empty[String]),
    add = (s: js.Any, x: String) => Ts.toJs(read[OrSet[String]]("orset.add", s).add(x, okay.Uid.system.next())),
    remove = (s: js.Any, x: String) => Ts.toJs(read[OrSet[String]]("orset.remove", s).remove(x)),
    has = (s: js.Any, x: String) => read[OrSet[String]]("orset.has", s).contains(x),
    values = (s: js.Any) => read[OrSet[String]]("orset.values", s).value.toVector.sorted.toJSArray,
    merge = (a: js.Any, b: js.Any) =>
      Ts.toJs(okay.crdt.Crdt[OrSet[String]].merge(read[OrSet[String]]("orset.merge", a), read[OrSet[String]]("orset.merge", b))),
  )

  // ---------------------------------------------------------------- channels

  /** an okay `Channel` of JSON values: `offer`, `close`, and `for await` */
  @JSExportTopLevel("channel")
  def channel(): js.Object & js.Dynamic =
    val c = Channel[Json]()
    val o = js.Dynamic.literal(
      offer = (x: js.Any) => c.offer(asJson(x)),
      close = () => c.close(),
    )
    val iterator: js.Function0[js.Object] = () => js.Dynamic.literal(
      next = () => Async.runAsync(c.receive).map {
        case Some(v) => js.Dynamic.literal(value = plain(v), done = false)
        case None => js.Dynamic.literal(value = js.undefined, done = true)
      }.toJSPromise)
    // a SYMBOL key, which a literal cannot name: Reflect.set, typed
    val _ = js.Dynamic.global.Reflect.set(o, js.Symbol.asyncIterator, iterator)
    o

  // ------------------------------------------------------------ declarations

  /** this package's `index.d.ts`, written from the Schemas above */
  @JSExportTopLevel("declarations")
  val declarations: String =
    val g = summon[Schema[GCounter]]
    val p = summon[Schema[PNCounter]]
    val s = summon[Schema[OrSet[String]]]
    Stubs.typescript(g, p, s) +
      s"""
export type GCounter = ${Stubs.typescriptType(g)};
export type PNCounter = ${Stubs.typescriptType(p)};
export type OrSet = ${Stubs.typescriptType(s)};

/** a program as data: okay walks it, and each perform is a callback */
export type Prog<T> =
  | { tag: "done"; value: T }
  | { tag: "perform"; name: string; args: unknown[]; k: (x: any) => Prog<T> };

export declare function done<T>(value: T): Prog<T>;
export declare function perform<T = unknown>(name: string, args: unknown[]): Prog<T>;
export declare function performing<T = unknown>(name: string, ...args: unknown[]): Prog<T>;
export declare function then<A, B>(p: Prog<A>, f: (a: A) => Prog<B>): Prog<B>;
/** walk a program; a callback answers a value or a Promise of one */
export declare function run<T>(program: Prog<T>, callbacks: Record<string, (...args: any[]) => unknown>): Promise<T>;

export declare const gcounter: {
  empty(): GCounter;
  inc(c: GCounter, node: string, by?: number): GCounter;
  merge(a: GCounter, b: GCounter): GCounter;
  value(c: GCounter): number;
};
export declare const pncounter: {
  empty(): PNCounter;
  inc(c: PNCounter, node: string, by?: number): PNCounter;
  dec(c: PNCounter, node: string, by?: number): PNCounter;
  merge(a: PNCounter, b: PNCounter): PNCounter;
  value(c: PNCounter): number;
};
export declare const orset: {
  empty(): OrSet;
  add(s: OrSet, x: string): OrSet;
  remove(s: OrSet, x: string): OrSet;
  has(s: OrSet, x: string): boolean;
  values(s: OrSet): string[];
  merge(a: OrSet, b: OrSet): OrSet;
};

/** where a durable flow's answers are kept: one entry per step, as JSON text */
export interface Journal {
  load(flow: string): Promise<string[]>;
  append(flow: string, step: number, entry: string): Promise<void>;
  clear(flow: string): Promise<void>;
}
/** in this page only */
export declare function memoryJournal(): Journal;
/** the browser's IndexedDB, database `name`: survives a reload */
export declare function indexedDbJournal(name: string): Journal;
/** run, with each answer journalled before the flow continues: a reload resumes it, and a
 * recorded step asked differently is refused as Drift */
export declare function durable<T>(flow: string, program: Prog<T>,
  callbacks: Record<string, (...args: any[]) => unknown>, journal: Journal): Promise<T>;

/** an okay Channel: offer answers whether it was taken; for await reads until close */
export interface Chan<T> extends AsyncIterable<T> {
  offer(x: T): boolean;
  close(): void;
}
export declare function channel<T>(): Chan<T>;

/** this file, as the module writes it */
export declare const declarations: string;
"""
