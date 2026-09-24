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
 * WHAT IT COMPARES is each part's CLASS, after flattening the
 * intersection. The same part twice (`State[Int] + State[Int]`) is one
 * requirement, not a collision. An ABSTRACT part (a row variable `F`
 * in generic code) is allowed: nothing can be said about it, and
 * row-generic code is written against exactly those — the Scala 3
 * core draws the line in the same place.
 *
 * WHY A MACRO: an inductive implicit over an intersection diverges in
 * scalac 2 (stage 8, `Replayable`), so the row is read whole.
 */
@implicitNotFound("the row ${R} holds TWO SIGNATURES OF ONE CLASS, which no runtime test can tell apart.")
final class Distinct[R <: Row] private ()

object Distinct {
  implicit def derive[R <: Row]: Distinct[R] = macro DistinctMacro.derive[R]

  /** THE ESCAPE HATCH: a row the macro cannot see as it is, or a test
   * finer than its class. You promise what the macro otherwise proves;
   * say in a comment which it is */
  def unchecked[R <: Row]: Distinct[R] = new Distinct[R]()
}

object DistinctMacro {
  def derive[R: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    def parts(t: Type): List[Type] = t.dealias match {
      case RefinedType(ps, _) => ps.flatMap(parts)
      case other => List(other)
    }
    val row = weakTypeOf[R]
    // a concrete signature: a class; an abstract part is a type
    // parameter or abstract member, and `Row` itself is the empty row
    val concrete = parts(row).filter(p => p.typeSymbol.isClass && p.typeSymbol.fullName != "okay2.Row")
    val distinct = concrete.foldLeft(List.empty[Type])((acc, p) => if (acc.exists(_ =:= p)) acc else acc :+ p)
    val clashes = distinct.groupBy(_.typeSymbol).filter(_._2.size > 1).values.toList
    if (clashes.nonEmpty)
      c.abort(c.enclosingPosition,
        "the row " + row + " holds TWO SIGNATURES OF ONE CLASS: " + clashes.map(_.mkString(" and ")).mkString("; ") +
          ".\nA split tells signatures apart by their class, so the second's operations would reach the first's handler" +
          " — a ClassCastException at the first wrong answer." +
          "\nKeep one of them in the row, or, where the test is finer than the class, say so: `Distinct.unchecked`.")
    q"_root_.okay2.Distinct.unchecked[$row]"
  }
}
