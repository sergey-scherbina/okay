package okay.agent

import okay.rag.Embedding
import scala.collection.immutable.ArraySeq

/**
 * The linear probe, without a server.
 *
 * Every other test of this tier is tagged Live because it embeds real
 * sentences; these use vectors made by hand, which is enough for the
 * questions that are about the CLASSIFIER rather than about the
 * representation: does it separate what is separable, and does it
 * report what it considered as well as what it chose.
 */
class TestProbe extends munit.FunSuite {

  def vec(xs: Float*): Embedding = ArraySeq.from(xs)

  /** three classes, each a corner — separable by construction, so any
   * failure here is the fitting and not the data */
  val corners = Vector(
    (vec(1f, 0f, 0f), "east"), (vec(0.9f, 0.1f, 0f), "east"),
    (vec(0f, 1f, 0f), "north"), (vec(0.1f, 0.9f, 0f), "north"),
    (vec(0f, 0f, 1f), "up"), (vec(0f, 0.1f, 0.9f), "up"))

  test("what it considered, not only what it chose") {
    val t = Probe.train(corners)
    val r = Probe.ranked(t, vec(1f, 0f, 0f))
    // EVERY class, which is what a diagnostic listing them needs and
    // what a caller would otherwise re-implement this softmax to get
    assertEquals(r.map(_._1).toSet, Set("east", "north", "up"))
    assertEquals(r, r.sortBy(-_._2))
    assertEqualsDouble(r.map(_._2).sum, 1.0, 1e-9)
    assertEquals(r.head._1, "east")
  }

  test("the verdict is the ranking's head, and its margin the first gap") {
    val t = Probe.train(corners)
    val v = vec(0f, 1f, 0f)
    val r = Probe.ranked(t, v)
    val said = Probe.score(t, v).getOrElse(fail("no verdict on a fitted probe"))
    assertEquals(said.best, r.head._1)
    assertEqualsDouble(said.probability, r.head._2, 1e-12)
    assertEqualsDouble(said.margin, r.head._2 - r(1)._2, 1e-12)
    assertEquals(said.runnerUp, Some(r(1)._1))
  }

  test("an unfitted probe ranks nothing rather than guessing") {
    val empty = Probe.train(Nil)
    assertEquals(Probe.ranked(empty, vec(1f, 0f, 0f)), Vector.empty)
    assertEquals(Probe.score(empty, vec(1f, 0f, 0f)), None)
  }

  test("a point between two classes has the small margin that says so") {
    val t = Probe.train(corners)
    val onACorner = Probe.score(t, vec(1f, 0f, 0f)).get.margin
    val betweenTwo = Probe.score(t, vec(0.7f, 0.7f, 0f)).get.margin
    assert(betweenTwo < onACorner,
      s"a point between two classes was as confident as one on a corner: $betweenTwo vs $onACorner")
  }
}
