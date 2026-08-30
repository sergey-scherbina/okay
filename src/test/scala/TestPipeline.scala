package okay

import org.scalacheck.{Gen as G}
import org.scalacheck.Prop.forAll

/** The optimizer: every rewrite preserves semantics; pushdowns pay. */
class TestPipeline extends munit.ScalaCheckSuite {

  /** random pipelines over a bounded range source */
  def pipelines: G[Pipeline[Long]] =
    val src = G.const(Pipeline.range(0, 200, 16))
    def ops(p: Pipeline[Long], depth: Int): G[Pipeline[Long]] =
      if depth == 0 then G.const(p)
      else G.oneOf(
        G.const(p.map(_ * 2)), G.const(p.map(_ + 3)),
        G.const(p.filter(_ % 3 != 0)), G.const(p.filter(_ % 2 == 0)),
        G.choose(0, 50).map(p.take), G.choose(0, 20).map(p.drop),
        G.choose(1, 40).map(p.rechunk),
      ).flatMap(ops(_, depth - 1))
    src.flatMap(ops(_, 6))

  property("optimize preserves semantics on random pipelines") {
    forAll(pipelines) { p =>
      Pipeline.fold(p)(using Fold[Long, List[Long]](Nil)((s, a) => s :+ a)) ==
        Chunks.fold(Pipeline.chunks(p))(using Fold[Long, List[Long]](Nil)((s, a) => s :+ a))
    }
  }

  test("fusion shrinks the tree") {
    val p = Pipeline.range(0, 100).map(_ + 1).map(_ * 2).filter(_ > 4).filter(_ % 2 == 0)
    assertEquals(Pipeline.depth(p), 5)
    assertEquals(Pipeline.depth(Pipeline.optimize(p)), 3)   // range, one map, one filter
  }

  test("take pushes into a range: construction is O(n)") {
    var built = 0
    val p = Pipeline.generate(0)(x => { built += 1; x })(_ + 1, 16).take(10)
    Pipeline.fold(p)(using Fold.count)
    val builtLazy = built
    // even unoptimized this source is lazy; the RANGE pushdown is structural:
    val r = Pipeline.optimize(Pipeline.range(0, 1000000).take(5))
    assertEquals(r, Pipeline.NumRange(0, 5, 64))
    assertEquals(builtLazy, 16)   // one chunk for ten elements
  }

  test("rechunk collapses into the source") {
    val p = Pipeline.optimize(Pipeline.range(0, 100, 64).rechunk(8).rechunk(4))
    assertEquals(p, Pipeline.NumRange(0, 100, 4))
  }

  test("the compiled pipeline agrees with the hand-written one") {
    val viaTree = Pipeline.fold(
      Pipeline.generate(0)(identity)(_ + 1).map(_ * 2).filter(_ % 3 == 0).take(1000))(
      using Fold.sum[Int])
    val byHand = Chunks.fold(
      Chunks.take(Chunks.filter(Chunks.map(Chunks.nats[Int]())(_ * 2))(_ % 3 == 0))(1000))(
      using Fold.sum[Int])
    assertEquals(viaTree, byHand)
  }
}
