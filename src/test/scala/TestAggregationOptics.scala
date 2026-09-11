package okay

/**
 * The aggregating families (specs/optics.md stage 4): a kaleidoscope
 * collapses many focuses through an Applicative, an algebraic lens
 * decides an answer from the whole dataset. The two compose, and the
 * composite is the literature's own example — aggregate, then
 * classify — written on okay's optics and okay's Aggregator.
 */
class TestAggregationOptics extends munit.FunSuite {

  // the zip applicative is passed explicitly everywhere: it is not a
  // given, because a sequence's cartesian applicative is the usual one
  val eachColumn: Kaleidoscope[LazyList[Double], LazyList[Double], Double, Double] =
    Kaleidoscope.each[LazyList, Double, Double](using Optic.zipLazy)

  val rows: Vector[LazyList[Double]] =
    Vector(LazyList(1.0, 2, 3, 4), LazyList(10.0, 20, 30, 40), LazyList(100.0, 200, 300, 400))

  def mean(xs: Vector[Double]): Double = xs.sum / xs.size

  test("a kaleidoscope aggregates POSITION-WISE: three rows become the columns' means") {
    assertEquals(eachColumn.aggregate(mean)(rows).toVector, Vector(37.0, 74.0, 111.0, 148.0))
  }

  test("the same, said with okay's own aggregation algebra") {
    assertEquals(eachColumn.aggregateWith(Aggregator.mean[Double])(rows).toVector,
      Vector(37.0, 74.0, 111.0, 148.0))
  }

  test("aggregating ONE whole by its only focus gives that whole back") {
    val one = LazyList(1.0, 2.0, 3.0)
    assertEquals(eachColumn.aggregate(_.head)(Vector(one)).toVector, one.toVector)
  }

  // ---------------------------------------------------------------- classifying

  final case class Point(label: String, xs: LazyList[Double])

  val dataset: Vector[Point] = Vector(
    Point("low", LazyList(1.0, 1.0)),
    Point("mid", LazyList(5.0, 5.0)),
    Point("high", LazyList(9.0, 9.0)))

  def nearest(ds: Vector[Point], xs: LazyList[Double]): String =
    ds.minBy(p => p.xs.zip(xs).map((a, b) => (a - b) * (a - b)).sum).label

  /** view the measurements; put by deciding what the measurement IS,
   * against everything measured — the paper's classifying lens */
  val measured: AlgebraicLens[Point, Point, LazyList[Double], LazyList[Double]] =
    AlgebraicLens(_.xs, (ds, xs) => Point(nearest(ds, xs), xs))

  test("an algebraic lens classifies against the whole dataset, not against a value") {
    val got = measured.aggregate(xss => xss.head)(dataset)
    assertEquals(got.label, "low")           // the head measurement IS the low point
    assertEquals(got.xs.toVector, Vector(1.0, 1.0))
  }

  test("classify AFTER aggregate: the composite is one optic") {
    val composite = measured.andThen(eachColumn)
    val got = composite.aggregate(mean)(dataset)
    assertEquals(got.xs.toVector, Vector(5.0, 5.0))   // the columns' means
    assertEquals(got.label, "mid")                     // and what that is
  }

  test("the composite agrees with doing both halves by hand") {
    val byHand = {
      val columns = eachColumn.aggregate(mean)(dataset.map(_.xs))
      Point(nearest(dataset, columns), columns)
    }
    val byOptic = measured.andThen(eachColumn).aggregate(mean)(dataset)
    assertEquals(byOptic.label, byHand.label)
    assertEquals(byOptic.xs.toVector, byHand.xs.toVector)
  }

  test("an ordinary lens does NOT reach the aggregating road, and the algebraic one does") {
    // `first` would have to answer a C from a Vector of Cs. There is no
    // honest choice, so `Aggregating` is not Strong — and the classifying
    // lens is what stands in a lens's place here (Clarke et al. 2024).
    val why = scala.compiletime.testing.typeCheckErrors(
      """Lens[Int, Int, Int, Int](i => i, (_, b) => b).aggregate(_.sum)""")
    assert(why.nonEmpty, "a Strong-only lens must not compile at Aggregating")
    assert(why.exists(_.message.contains("Reflecting")) ||
           why.exists(_.message.contains("Classifying")) ||
           why.exists(_.message.contains("Strong")),
      s"it must fail for the MISSING INSTANCE, not for a typo: ${why.map(_.message)}")
    assert(scala.compiletime.testing.typeChecks(
      """AlgebraicLens[Int, Int, Int, Int](i => i, (is, b) => is.sum + b).aggregate(_.sum)"""),
      "the algebraic lens must compile at Aggregating")
  }
}
