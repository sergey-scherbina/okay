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

/** delimit, and answer every capture of answer `R` */
def reset[R, F[+_]](body: R ! Shift % R + F)(using k: Shift.Key[R], d: Distinct[Shift % R + F], n: Shift.Nesting[F]): R ! F =
  val pushed = Delim.push[R, F](k.prompt)(Shift.in(body))
  // a row that still holds a capture's effect is run by the machine outside
  if n.inner then Shift.inner(pushed) else Delim.run[R, F](pushed)

/** `reset` as a value: `p.handle(Reset[R])` */
object Reset:
  def apply[R](using k: Shift.Key[R]): Handling[Shift % R, R, [A] =>> R, Shift.Nesting] =
    new Handling[Shift % R, R, [A] =>> R, Shift.Nesting]:
      def run[A, F[+_]](p: A ! Shift % R + F)(using a: A <:< R, d: Distinct[Shift % R + F], n: Shift.Nesting[F]): R ! F =
        reset[R, F](a.substituteCo[[X] =>> X ! Shift % R + F](p))

object Shift:

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
