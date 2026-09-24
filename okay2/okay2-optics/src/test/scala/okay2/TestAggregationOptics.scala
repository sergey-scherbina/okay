package okay2

import okay2.Optic._
import OpticsFixtures._

/** The aggregating families — the Scala 3 core's TestAggregationOptics:
 * a kaleidoscope collapses many focuses through an Applicative, an
 * algebraic lens decides from the whole dataset, and the two compose */
class TestAggregationOptics extends munit.FunSuite {

  val eachColumn: Kaleidoscope[LazyList[Double], LazyList[Double], Double, Double] =
    Kaleidoscope.each[LazyList, Double, Double](Optic.zipLazy)

  val rows: Vector[LazyList[Double]] =
    Vector(LazyList(1.0, 2, 3, 4), LazyList(10.0, 20, 30, 40), LazyList(100.0, 200, 300, 400))

  def mean(xs: Vector[Double]): Double = xs.sum / xs.size

  test("a kaleidoscope aggregates POSITION-WISE: three rows become the columns' means") {
    assertEquals(eachColumn.aggregate(mean)(rows).toVector, Vector(37.0, 74.0, 111.0, 148.0))
    assertEquals(eachColumn.aggregateWith(Aggregator.mean[Double])(rows).toVector, Vector(37.0, 74.0, 111.0, 148.0))
  }

  test("aggregating ONE whole by its only focus gives that whole back") {
    val one = LazyList(1.0, 2.0, 3.0)
    assertEquals(eachColumn.aggregate(_.head)(Vector(one)).toVector, one.toVector)
  }

  val dataset: Vector[Point] = Vector(Point("low", LazyList(1.0, 1.0)), Point("mid", LazyList(5.0, 5.0)), Point("high", LazyList(9.0, 9.0)))

  def nearest(ds: Vector[Point], xs: LazyList[Double]): String =
    ds.minBy(p => p.xs.zip(xs).map { case (a, b) => (a - b) * (a - b) }.sum).label

  val measured: AlgebraicLens[Point, Point, LazyList[Double], LazyList[Double]] =
    AlgebraicLens[Point, Point, LazyList[Double], LazyList[Double]](_.xs, (ds, xs) => Point(nearest(ds, xs), xs))

  test("an algebraic lens classifies against the whole dataset, not against a value") {
    val got = measured.aggregate(xss => xss.head)(dataset)
    assertEquals(got.label, "low")
  }

  test("classify AFTER aggregate: the composite is one optic, and agrees with both halves by hand") {
    val got = measured.andThen(eachColumn).aggregate(mean)(dataset)
    assertEquals(got.xs.toVector, Vector(5.0, 5.0))
    assertEquals(got.label, "mid")
    val columns = eachColumn.aggregate(mean)(dataset.map(_.xs))
    assertEquals(got.label, nearest(dataset, columns))
  }

  test("an ordinary lens does NOT reach the aggregating road, and the algebraic one does") {
    assert(compileErrors("okay2.Optic.Lens[Int, Int, Int, Int](i => i, (_, b) => b).aggregate(_.sum)").nonEmpty,
      "a Strong-only lens must not compile at Aggregating")
    assertEquals(compileErrors("import okay2.Optic._; AlgebraicLens[Int, Int, Int, Int](i => i, (is, b) => is.sum + b).aggregate(_.sum)"), "")
  }
}
