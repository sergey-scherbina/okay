package okay

import scala.quoted.*

// The shift-effect probe's two macros (specs/shift-effect.md). They live in main only
// because a macro must be compiled before the sources that expand it: in the
// test sources zinc fails on the suspended run ("Failed to find name hashes").
// Package-private: not an API until the operator decides level 1's.

/**
 * specs/shift-effect.md: the key of an answer type, made at compile time.
 * Two types, two keys; one type (through any alias, a union in any order),
 * one key. An abstract type has none here: it is passed in, as a `ClassTag` is.
 */
private[okay] final class Key[R](val id: String):
  override def toString: String = id

private[okay] object Key:
  inline given of[R]: Key[R] = ${ impl[R] }

  def impl[R: Type](using q: Quotes): Expr[Key[R]] =
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
          s"the answer type ${Type.show[R]} is abstract here (${other.show}), so it has no key of its own; " +
            s"take a `Key[${other.show}]` as a parameter where it is known")
    val id = Expr(norm(TypeRepr.of[R]))
    '{ new Key[R]($id) }

/**
 * Whether a row still holds a `Shift` of some answer type, read off the
 * row at compile time: a `reset` over such a row is INNER, and on Delim's
 * machine it pushes its prompt on the machine an outer `reset` runs. An
 * abstract row reads as outer.
 */
private[okay] final class Nesting[F[+_]](val inner: Boolean)

private[okay] object Nesting:
  inline given of[F[+_]]: Nesting[F] = ${ impl[F] }

  def impl[F[+_]: Type](using q: Quotes): Expr[Nesting[F]] =
    import q.reflect.*
    val shift = Symbol.requiredClass("okay.Shift")
    def members(t: TypeRepr): List[TypeRepr] = t.dealias.simplified match
      case OrType(a, b) => members(a) ++ members(b)
      case other => List(other)
    val applied = TypeRepr.of[F].appliedTo(TypeRepr.of[Any])
    val inner = members(applied).exists(_.typeSymbol == shift)
    '{ new Nesting[F](${ Expr(inner) }) }
