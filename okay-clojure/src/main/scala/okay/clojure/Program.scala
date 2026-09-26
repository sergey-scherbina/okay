package okay.clojure

import okay.{!, %, +, Chunk, ChunkBuf, Chunks, Foreign, Free, Member, Pure, Stage, Take, Writer, pure}
import clojure.lang.{AFn, Cons, IFn, ILookup, ISeq, Keyword, LazySeq, RT}
import scala.reflect.ClassTag

/**
 * Clojure programs as okay programs (specs/clojure.md, stage 2).
 *
 * `okay.core` — a Clojure namespace shipped in this jar — makes a program
 * DATA: `(done v)`, or `(step op k)` with `k` a Clojure function from the
 * operation's answer to the rest. This driver walks it: `await`/`tell`
 * become a stage's own, `perform op` an operation of the row F, every
 * step one okay program node (so the walk is trampolined), and `k` is
 * called with the answer — per branch, when a handler resumes more than
 * once. The same walk okay-frege makes over its Frege `Prog`.
 */
object Program {

  // okay.core's shapes: its records, recognised by class once loaded
  private lazy val loaded: Unit = Clj.require("okay.core").fold(e => throw IllegalStateException(e), identity)
  private lazy val doneClass: Class[?] = { loaded; RT.classForName("okay.core.Done") }
  private lazy val tellClass: Class[?] = { loaded; RT.classForName("okay.core.Tell") }
  private lazy val liftClass: Class[?] = { loaded; RT.classForName("okay.core.Lift") }
  private val kF = Keyword.intern(null, "f")
  private val kValue = Keyword.intern(null, "value")
  private val kOp = Keyword.intern(null, "op")
  private val kK = Keyword.intern(null, "k")
  private val awaitOp = Keyword.intern("okay.core", "await")

  private def field(record: AnyRef, key: Keyword): AnyRef = record match
    case l: ILookup => l.valAt(key)
    case other => throw IllegalArgumentException(
      s"okay.clojure: expected an okay.core program, got ${if other == null then "nil" else other.getClass.getName}")

  private def continuation(step: AnyRef): IFn = field(step, kK) match
    case f: IFn => f
    case other => throw IllegalArgumentException(s"okay.clojure: a step's continuation is not a function: $other")

  private def as[T: ClassTag](x: Any, what: String): T = Foreign.as[T](x, what, "okay.clojure")

  private def boxed(a: Any): AnyRef = Foreign.obj(a)

  /** okay.core's records, read for okay-stream's `Foreign` walker
   * (interop-shared): `(done v)`, `(step op k)` with `op` the keyword
   * `await`, a `Tell` record, or an operation for the row */
  private given View: Foreign.View[AnyRef] with
    def kind(p: AnyRef): Int =
      if doneClass.isInstance(p) then Foreign.Done
      else
        val op = field(p, kOp)
        if op eq awaitOp then Foreign.Await
        else if tellClass.isInstance(op) then Foreign.Tell
        else if liftClass.isInstance(op) then Foreign.Lift
        else Foreign.Perform
    def payload(p: AnyRef): AnyRef =
      if doneClass.isInstance(p) then field(p, kValue)
      else
        val op = field(p, kOp)
        if tellClass.isInstance(op) then field(op, kValue) else op
    /** `(ok/lift f)`: call `f`, in place or on its own thread */
    def lift(p: AnyRef): AnyRef = field(field(p, kOp), kF) match
      case f: IFn => f.invoke()
      case other => throw IllegalArgumentException(s"okay.clojure: a lift's action is not a function: $other")
    /** on a thread of its own, which a cancel interrupts, when the row
     * carries Async (interop-lift-cancellation) */
    override def liftAsOperation(p: AnyRef): Option[AnyRef] = Some(okay.Interruptible.await(() => lift(p)))
    def resume(p: AnyRef, answer: AnyRef): AnyRef = continuation(p).invoke(answer)
    def who = "okay.clojure"

  /**
   * Whether a value from Clojure is an operation of the row F: the core's
   * `okay.Member` (interop-shared), found for one signature, built with
   * `|` for a union — `Program.Row.of[Reader % Long] | Program.Row.of[State % Long]`.
   */
  type Row[F[+_]] = Member[F]
  val Row: Member.type = Member

  /** a Clojure stage that awaits and tells, as an okay `Stage` */
  def stage[I, O: ClassTag](prog: => AnyRef, name: String = "a Clojure stage"): Stage[I, O, Unit] =
    stageWith[I, O, Pure](prog, name)

  /**
   * A Clojure stage that also PERFORMS operations of F — okay-stream's
   * effectful stage row, so it composes through `through` like any.
   */
  def stageWith[I, O: ClassTag, F[+_]](prog: => AnyRef, name: String = "a Clojure stage")
                                     (using Row[F]): Unit ! (Take % I + (Writer % O + F)) =
    Foreign.stageWith[I, O, F, AnyRef](prog, name)

  /**
   * A Clojure program as `A ! F`: each `perform` an operation of F under
   * whatever handlers run the result; `await`/`tell` belong to a stage.
   */
  def run[F[+_], A: ClassTag](prog: => AnyRef, name: String = "a Clojure program",
                             calls: Foreign.Calls[F] = Foreign.Calls.none[F])(using Row[F]): A ! F =
    Foreign.run[F, A, AnyRef](prog, name, calls)

  // ------------------------------------------------------------ data

  /**
   * A Clojure seq (anything `seq` accepts: a lazy seq, a vector, a range)
   * as okay `Chunks`, LAZILY: each chunk realises as many elements as it
   * holds, when okay pulls it — an infinite `(range)` is fine. Each run of
   * the result walks the seq from its head again (a realised seq is an
   * immutable, cached value).
   */
  def chunks[A: ClassTag](coll: => AnyRef, size: Int = 64): Chunks[A] =
    def go(rest: ISeq | Null): Chunks[A] = Free.delay { () =>
      val buf = ChunkBuf[A](size)
      var cur = rest
      var i = 0
      while i < size && cur != null do
        buf(i) = as[A](cur.nn.first(), "a Clojure seq's element")
        cur = cur.nn.next()
        i += 1
      if i == 0 then pure(())
      else
        val next = cur
        Writer.tell(buf.take(i)).flatMap(_ => go(next))
    }
    Free.delay(() => go(RT.seq(coll)))

  /**
   * okay `Chunks` as a Clojure LAZY seq: an element is realised when
   * Clojure asks for it, the next chunk pulled only past the last element
   * of the current one. PURE by type — only `Chunks` becomes a seq — for
   * the lazy-IO reason (specs/frege.md states it once for both bridges).
   */
  def seq[A](c: Chunks[A]): ISeq =
    def cells(chunk: Chunk[A], i: Int, rest: Chunks[A]): ISeq | Null =
      if i < chunk.length then Cons(boxed(chunk(i)), LazySeq(new AFn:
        override def invoke(): AnyRef = cells(chunk, i + 1, rest)))
      else Chunks.pull(rest) match
        case Some((next, more)) => cells(next, 0, more)
        case None => null
    LazySeq(new AFn:
      override def invoke(): AnyRef = cells(Chunks.emptyChunk[A], 0, c))
}
