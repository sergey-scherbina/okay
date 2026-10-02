package okay

import okay.Row.plus
import scala.quoted.*

/**
 * A continuation as an effect (specs/shift-effect.md): `Shift % R` in the row is a capture to the nearest
 * `reset` of answer `R`, and `reset` is its handler. Its operations are Delim's and run on Delim's machine;
 * no value of this type is made.
 */
sealed trait Shift[R, +A]

/** Danvy-Filinski's capture: the body runs under its `reset`, so it may capture to it again; `k` re-installs it */
def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
  Shift.out(Delim.shift[R, A, F](k.prompt)(Shift.clause(f)))

/** the body runs outside its `reset`; `k` re-installs it */
def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using k: Shift.Key[R], at: At): A ! Shift % R + F =
  Shift.out(Delim.shift0[R, A, F](k.prompt)(Shift.clause(f)))

/** inside a `reset` block: `R` and `F` are the block's, so only the value type is named */
def shift[A](using in: Shift.In)(f: (A => in.R ! Shift % in.R + in.F) => in.R ! Shift % in.R + in.F)(using at: At): A ! Shift % in.R + in.F =
  shift[in.R, A, in.F](f)(using in.key, at)

/** inside a `reset` block, the 0-variant */
def shift0[A](using in: Shift.In)(f: (A => in.R ! in.F) => in.R ! in.F)(using at: At): A ! Shift % in.R + in.F =
  shift0[in.R, A, in.F](f)(using in.key, at)

/**
 * delimit, and answer every capture of answer `R`. The body sees a `Shift.In[R, F]`, so a `shift` written in it
 * names only its value type; a program built elsewhere passes as it is.
 */
def reset[R, F[+_]](body: Shift.In.Aux[R, F] ?=> R ! Shift % R + F)
                   (using k: Shift.Key[R], d: Distinct[Shift % R + F], n: Shift.Nesting[F]): R ! F =
  val pushed = Delim.push[R, F](k.prompt)(Shift.in(body(using Shift.In[R, F](k))))
  // a row that still holds a capture's effect is run by the machine outside
  if n.inner then Shift.inner(pushed) else Shift.run[R, F](pushed)

/** `reset` as a value: `p.handle(Reset[R])` */
object Reset:
  def apply[R](using k: Shift.Key[R]): Handling[Shift % R, R, [A] =>> R, Shift.Nesting] =
    new Handling[Shift % R, R, [A] =>> R, Shift.Nesting]:
      def run[A, F[+_]](p: A ! Shift % R + F)(using a: A <:< R, d: Distinct[Shift % R + F], n: Shift.Nesting[F]): R ! F =
        reset[R, F](a.substituteCo[[X] =>> X ! Shift % R + F](p))

object Shift:

  /**
   * A `reset` that runs its own machine runs it INSIDE whatever forced it, and nested resets of one answer type
   * each start one: JVM depth grows with the nesting (3 000-10 000 deep, then StackOverflowError). So the runs
   * are counted per thread, and past the room the next one runs on a fresh stack, as Cont's strict `k` does
   * (StackSwitch, specs/cont-stack.md Layer 2). A level is taken as ~4 KB cold, Cont's ~1.2 KB scaled.
   */
  private val room: Int = Integer.getInteger("okay.shift.room", math.max(32L, StackSwitch.firstRoom.toLong * 1200 / 4096).toInt)
  private val left: ThreadLocal[Array[Int]] = ThreadLocal.withInitial(() => Array(room))

  /** run the machine for one `reset`, one level less of room; at zero on a fresh stack */
  private[okay] def run[R, F[+_]](pushed: R ! Delim + F): R ! F =
    val cell = left.get
    val here = cell(0)
    if here > 0 then
      cell(0) = here - 1
      try Delim.run[R, F](pushed) finally cell(0) = here
    else StackSwitch.fresh { big =>
      val c = left.get
      val saved = c(0)
      c(0) = big / 2
      try Delim.run[R, F](pushed) finally c(0) = saved
    }

  /** evidence of an enclosing `reset` block: its answer `R`, and `F`, the row outside it */
  @scala.annotation.implicitNotFound("no reset around this shift: inside `reset { … }` a shift names only its value type, `shift[A](k => …)`; elsewhere name all three, `shift[R, A, F](k => …)`")
  sealed trait In:
    type R
    type F[+_]
    def key: Key[R]

  object In:
    type Aux[R0, F0[+_]] = In { type R = R0; type F[+X] = F0[X] }
    def apply[R0, F0[+_]](k: Key[R0]): Aux[R0, F0] = new In:
      type R = R0
      type F[+X] = F0[X]
      def key: Key[R0] = k

  // THE ONE CLAIM: a `Shift % R` program is a `Delim` program at the same erasure (only the machine reads
  // `Cont0`), and a capture of answer `R` reaches only the prompt of `R`'s key, where its `k` and body are
  // typed in that `reset`'s row.
  private[okay] def in[A, R, F[+_]](q: A ! Shift % R + F): A ! Delim + F = q.asInstanceOf[A ! Delim + F]
  private[okay] def out[A, R, F[+_]](q: A ! Delim + F): A ! Shift % R + F = q.asInstanceOf[A ! Shift % R + F]
  private[okay] def inner[R, F[+_]](q: R ! Delim + F): R ! F = q.asInstanceOf[R ! F]
  private[okay] def clause[R, A, F[+_], G[+_]](f: (A => R ! G) => R ! G): (A => R ! Delim + F) => R ! Delim + F =
    f.asInstanceOf[(A => R ! Delim + F) => R ! Delim + F]

  /** the test reads the prompt, so `Shift % Int + Shift % String` is a good row */
  given typeableK[R](using k: Key[R]): TypeableK.ByValue[Shift % R] = new:
    def test(x: Any): Boolean = x match
      case s: Cont0.Shift0[?, ?, ?, ?, ?, ?] => (s.p: AnyRef) eq k.prompt
      case d: Cont0.Dollar0[?, ?, ?, ?, ?] => (d.p: AnyRef) eq k.prompt
      case _ => false

  /** level 2: the program as a `Cont` whose answers are programs: `c / k` is `reset(q >>= k)` */
  def cont[A, R, F[+_]](q: A ! Shift % R + F)(using Key[R], Distinct[Shift % R + F], Nesting[F]): Cont[A, R ! F, R ! F] =
    Cont.shift[A, R ! F, R ! F](k => reset[R, F](q.flatMap(a => k(a).plus[Shift % R])))

  /** level 2: a whole `Cont` as one capture */
  def embed[A, R, F[+_]](c: Cont[A, R ! F, R ! F])(using Key[R], At): A ! Shift % R + F =
    shift0[R, A, F](k => c / k)

  /**
   * The key of an answer type, made at compile time: two types, two keys; one type (through any alias, a union
   * in either order), one key, and one prompt for it. An abstract type has none: it is passed in, as a
   * `ClassTag` is.
   */
  final class Key[R] private (val id: String, private[okay] val prompt: Prompt[R]):
    override def toString: String = id

  object Key:
    private val keys = new java.util.concurrent.ConcurrentHashMap[String, Key[Any]]

    /** the key of `id`, one per id */
    def intern[R](id: String): Key[R] =
      val k = keys.get(id)
      // one key per id, made at `Any` and read back at the type the id names
      (if k != null then k else keys.computeIfAbsent(id, i => new Key[Any](i, new Prompt[Any]("reset", i)))).asInstanceOf[Key[R]]

    inline given of[R]: Key[R] = ${ keyImpl[R] }

  def keyImpl[R: Type](using q: Quotes): Expr[Key[R]] =
    import q.reflect.*
    def parts(t: TypeRepr, or: Boolean): List[TypeRepr] = t.dealias match
      case OrType(a, b) if or => parts(a, or) ++ parts(b, or)
      case AndType(a, b) if !or => parts(a, or) ++ parts(b, or)
      case other => List(other)
    // bounded by the type's own nesting, which the compiler has already walked
    def norm(t: TypeRepr): String = t.dealias.simplified match
      case o: OrType => parts(o, or = true).map(norm).distinct.sorted.mkString("(", " | ", ")")
      case a: AndType => parts(a, or = false).map(norm).distinct.sorted.mkString("(", " & ", ")")
      case AppliedType(c, args) => norm(c) + args.map(norm).mkString("[", ", ", "]")
      case c: ConstantType => c.show
      case other =>
        val s = other.typeSymbol
        if s.isClassDef || s.flags.is(Flags.Opaque) then s.fullName
        else report.errorAndAbort(
          s"the answer type ${Type.show[R]} is abstract here (${other.show}), so a reset or shift of it has no key; " +
            s"take a `Shift.Key[${other.show}]` as a parameter where the type is known")
    '{ Key.intern[R](${ Expr(norm(TypeRepr.of[R])) }) }

  /**
   * Whether a row still holds a capture's effect (`Shift` or `Delim`), read off the row at compile time: a
   * `reset` over such a row pushes its prompt on the machine an outer one runs. An abstract row reads as
   * none, so code generic in the row and nested in another `reset` passes its `Nesting` on.
   */
  final class Nesting[F[+_]] @scala.annotation.publicInBinary private[okay] (val inner: Boolean)

  object Nesting:
    inline given of[F[+_]]: Nesting[F] = ${ nestingImpl[F] }

  def nestingImpl[F[+_]: Type](using q: Quotes): Expr[Nesting[F]] =
    import q.reflect.*
    val shift = TypeRepr.of[Shift[Any, Any]].typeSymbol
    val delim = TypeRepr.of[Cont0[?, ?, ?, Any]].typeSymbol
    def members(t: TypeRepr): List[TypeRepr] = t.dealias.simplified match
      case OrType(a, b) => members(a) ++ members(b)
      case other => List(other)
    val inner = members(TypeRepr.of[F].appliedTo(TypeRepr.of[Any])).exists(m => m.typeSymbol == shift || m.typeSymbol == delim)
    '{ new Nesting[F](${ Expr(inner) }) }
