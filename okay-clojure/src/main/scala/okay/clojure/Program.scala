package okay.clojure

import okay.{!, %, +, Chunk, ChunkBuf, Chunks, Free, Pure, Stage, Take, TypeableK, Writer, effect, pure}
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

  /** a value from Clojure, as the type the okay side declared */
  private def as[T](x: Any, what: String)(using ct: ClassTag[T]): T = x match
    case ct(t) => t
    case other => throw IllegalArgumentException(
      s"okay.clojure: $what expected ${ct.runtimeClass.getName}, got " +
        (if other == null then "nil" else other.getClass.getName))

  /** an erased value handed to Clojure, which takes `Object`: it already
   * IS one at run time, so the ascription checks nothing and cannot fail */
  private def boxed(a: Any): AnyRef = a.asInstanceOf[AnyRef]

  /**
   * Whether a value from Clojure is an operation of the row F — found for
   * one signature (its `TypeableK`), built with `|` for a union, for the
   * reason okay-frege's `Frege.Row` states (dotty does not infer the two
   * sides of a union type lambda).
   */
  trait Row[F[+_]]:
    def test(x: Any): Boolean
    def |[G[+_]](g: Row[G]): Row[F + G] = x => test(x) || g.test(x)

  object Row:
    given one[F[+_]](using t: TypeableK[F]): Row[F] = x => t.test(x)
    def of[F[+_]](using t: TypeableK[F]): Row[F] = one[F]

  /** the one cast: refined only after the row's test said it IS an F
   * operation; F is covariant, so its answer needs none */
  private def operationOf[F[+_]](op: AnyRef)(using r: Row[F]): Option[F[Any]] =
    if r.test(op) then Some(op.asInstanceOf[F[Any]]) else None

  /** a Clojure stage that awaits and tells, as an okay `Stage` */
  def stage[I, O: ClassTag](prog: => AnyRef, name: String = "a Clojure stage"): Stage[I, O, Unit] =
    stageWith[I, O, Pure](prog, name)

  /**
   * A Clojure stage that also PERFORMS operations of F — okay-stream's
   * effectful stage row, so it composes through `through` like any.
   */
  def stageWith[I, O: ClassTag, F[+_]](prog: => AnyRef, name: String = "a Clojure stage")
                                     (using Row[F]): Unit ! (Take % I + (Writer % O + F)) =
    type R = Take % I + (Writer % O + F)
    def go(p: AnyRef): Unit ! R =
      if doneClass.isInstance(p) then pure(())
      else
        val op = field(p, kOp)
        val k = continuation(p)
        if op eq awaitOp then
          effect[R, Option[I]](Take.Await()).flatMap(in => go(k.invoke(in.fold(null)(boxed))))
        else if tellClass.isInstance(op) then
          effect[R, Unit](Writer(as[O](field(op, kValue), s"$name's tell"))).flatMap(_ => go(k.invoke(null)))
        else operationOf[F](op) match
          case Some(o) => effect[R, Any](o).flatMap(x => go(k.invoke(boxed(x))))
          case None => throw IllegalArgumentException(
            s"okay.clojure: $name performed ${if op == null then "nil" else op.getClass.getName}, which is not " +
              "an operation of this stage's row (a stage's own are await and tell; stageWith[I, O, F] adds F)")
    Free.delay(() => go(prog))

  /**
   * A Clojure program as `A ! F`: each `perform` an operation of F under
   * whatever handlers run the result; `await`/`tell` belong to a stage.
   */
  def run[F[+_], A: ClassTag](prog: => AnyRef, name: String = "a Clojure program")(using Row[F]): A ! F =
    def go(p: AnyRef): A ! F =
      if doneClass.isInstance(p) then pure(as[A](field(p, kValue), s"$name's answer"))
      else
        val op = field(p, kOp)
        val k = continuation(p)
        if (op eq awaitOp) || tellClass.isInstance(op) then throw IllegalStateException(
          s"okay.clojure: $name used ${if op eq awaitOp then "await" else "tell"} outside a stage; " +
            "Program.stage runs a program that awaits and tells")
        else operationOf[F](op) match
          case Some(o) => effect[F, Any](o).flatMap(x => go(k.invoke(boxed(x))))
          case None => throw IllegalArgumentException(
            s"okay.clojure: $name performed ${if op == null then "nil" else op.getClass.getName}, which is not " +
              "an operation of this program's row")
    Free.delay(() => go(prog))

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
