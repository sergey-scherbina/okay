package okay2

import scala.annotation.implicitNotFound
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * A ROW WHOSE MEMBERS CAN BE TOLD APART, checked by the compiler
 * (specs/okay2.md, stage 10; the Scala 3 core's `Distinct`).
 *
 * A split tests ONE signature by its class and takes the rest by
 * exclusion. That is sound exactly when no member's test accepts
 * another member's operations — and a signature's test is its class
 * (`Effect.of`), so two signatures of ONE class with different
 * parameters, `Ask[Int] + Ask[String]` or `State[Int] + State[String]`,
 * are two types to the row and one to the split: the second's
 * operations reach the first's handler, and the program dies of a
 * ClassCastException at the first wrong answer (TestDistinct measures
 * it). `Distinct[R]` moves that to compile time, where handlers are
 * composed: `Handler.union`, `Into.union`, `IntoZ.union` require it.
 *
 * WHAT IT COMPARES is each part's RUNTIME IDENTITY — what its test
 * looks at — after flattening the intersection (stage 11, the Scala 3
 * macro's model):
 * - a plain signature: its CLASS;
 * - `Tag[K, F]`: the literal key, then F's identity — two tags collide
 *   only under one key over colliding signatures;
 * - `Instances[F]`: F's identity under a handle;
 * - a part whose `TypeableK`, as the CALL SITE resolves it, is declared
 *   `TypeableK.ByValue` (`import okay2.Writer.byValue._`): alone.
 * The same part twice (`State[Int] + State[Int]`) is one requirement,
 * not a collision. An ABSTRACT part (a row variable `F` in generic
 * code) is allowed: nothing can be said about it, and row-generic code
 * is written against exactly those — the Scala 3 core draws the line in
 * the same place.
 *
 * WHY A MACRO: an inductive implicit over an intersection diverges in
 * scalac 2 (stage 8, `Replayable`), so the row is read whole.
 */
@implicitNotFound("the row ${R} holds two members no runtime test can tell apart.\nGive the instances an identity the split can see: a key (Tag[\"a\", F]), a run-time handle (Instances[F]), for Writer the finer test (import okay2.Writer.byValue._), or a Delim prompt.\ndocs/okay2.md, \"Several instances of one signature\", chooses between them.")
final class Distinct[R <: Row] private ()

object Distinct {
  implicit def derive[R <: Row]: Distinct[R] = macro DistinctMacro.derive[R]

  /**
   * THE ESCAPE HATCH: for a row the macro cannot see as it is — never
   * for two signatures of one class, which is the misrouting row itself
   * (the first cut of the macro's message recommended it there; with a
   * class test that is the ClassCastException the check exists for).
   * You promise what the macro otherwise proves; say in a comment which
   * it is.
   */
  def unchecked[R <: Row]: Distinct[R] = shared.asInstanceOf[Distinct[R]]

  /** THE ONE CAST, and why it is right: the witness carries no data, so
   * every `Distinct[R]` is the same object whatever R is; shared because
   * the handlers ask for one on every call */
  private val shared: Distinct[Row] = new Distinct[Row]()
}

object DistinctMacro {
  def derive[R: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._

    /** a part's RUNTIME identity: what its test actually looks at — the
     * Scala 3 macro's `Id`, the same four cases */
    sealed trait Id
    /** the test is the class — two of these with one class collide */
    final case class Cls(sym: Symbol) extends Id
    /** `Tag[K, F]`: the key first, then F's own identity */
    final case class Keyed(key: Any, inner: Id) extends Id
    /** `Instances[F]`: a handle at run time, F's identity under it */
    final case class Inst(inner: Id) extends Id
    /** a test that reads the value, or a part nothing can be said about */
    case object Alone extends Id

    val rowSym = typeOf[okay2.Row].typeSymbol
    val tagSym = typeOf[okay2.Tag[_, _]].typeSymbol
    val instSym = typeOf[okay2.Instances[_]].typeSymbol
    val testCtor = typeOf[okay2.TypeableK[_]].typeConstructor
    val byValueCtor = typeOf[okay2.TypeableK.ByValue[_]].typeConstructor

    def parts(t: Type): List[Type] = t.dealias match {
      case RefinedType(parents, _) => parents.flatMap(parts)
      case other => List(other)
    }

    /** does the part's own TypeableK, AS THE CALL SITE SEES IT, read the
     * value? `import okay2.Writer.byValue._` is what makes it so */
    def byValue(part: Type): Boolean = {
      val found = c.inferImplicitValue(appliedType(testCtor, part), silent = true)
      found != EmptyTree && found.tpe <:< appliedType(byValueCtor, part)
    }

    def identityOf(t0: Type): Id = {
      val t = t0.dealias
      val sym = t.typeSymbol
      if (!sym.isClass || sym == rowSym) Alone // abstract, or the empty row
      else if (sym == tagSym) t.typeArgs match {
        case List(k, f) => k.dealias match {
          case ConstantType(Constant(v)) => Keyed(v, identityOf(f))
          case _ => Alone // a key that is not a literal
        }
        case _ => Alone
      }
      else if (sym == instSym) Inst(identityOf(t.typeArgs.head))
      else if (byValue(t)) Alone
      else Cls(sym)
    }

    def collide(a: Id, b: Id): Boolean = (a, b) match {
      case (Cls(x), Cls(y)) => x == y
      case (Keyed(k, x), Keyed(l, y)) => k == l && collide(x, y)
      case (Inst(x), Inst(y)) => collide(x, y)
      case _ => false
    }

    val row = weakTypeOf[R]
    // the same part twice is one requirement, not a collision
    val ps = parts(row).foldLeft(List.empty[Type])((acc, p) => if (acc.exists(_ =:= p)) acc else acc :+ p)
    val ids = ps.map(identityOf)
    val clashes = for {
      i <- ps.indices
      j <- (i + 1) until ps.length
      if collide(ids(i), ids(j))
    } yield s"${ps(i)} and ${ps(j)}"
    if (clashes.nonEmpty)
      c.abort(c.enclosingPosition,
        "the row " + row + " holds members no runtime test can tell apart: " + clashes.mkString("; ") + ".\n" +
          "Both are tested by the CLASS of their operations, so the first handler to split the row answers the\n" +
          "second one's operations too, and the second continuation gets a ClassCastException at its first wrong answer.\n" +
          "\n" +
          "Give the instances an identity the split can see:\n" +
          "  Tag[\"a\", F] + Tag[\"b\", F]     a key named at compile time; Tag.tag/untag move a program in and out\n" +
          "  Instances[F]                   a handle made at run time, one row member for all of them\n" +
          "  import okay2.Writer.byValue._  for Writer: test the told value's class as well\n" +
          "  a Delim prompt                 a fresh identity per handler installation\n" +
          "docs/okay2.md, \"Several instances of one signature\", chooses between them.\n" +
          "If your signature's own TypeableK reads the operation's VALUE, declare it TypeableK.ByValue[YourSig].")
    q"_root_.okay2.Distinct.unchecked[$row]"
  }
}
