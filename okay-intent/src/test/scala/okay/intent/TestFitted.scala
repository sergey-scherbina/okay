package okay.intent

import okay.codec.{Json, Schema}
import okay.rag.{Embedding, embedding}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/**
 * A saved model is the same classifier (specs/intent-classify.md).
 *
 * Round-tripping the bytes is the easy half and not the point: what a
 * caller needs is that the model it LOADS answers exactly what the one
 * it fitted answered. So every property here compares PREDICTIONS
 * across the round trip, not fields.
 */
class TestFitted extends munit.ScalaCheckSuite {

  private def vec(xs: Double*): Embedding = embedding(xs.map(_.toFloat).toArray)

  private val rows: Seq[(Embedding, String)] = (0 until 40).map { i =>
    val a = i % 2 == 0
    (vec(if a then 1.0 else -1.0, (i % 5) * 0.1, 0.3), if a then "A" else "B")
  }

  private val texts: Seq[(String, String)] = (0 until 40).map { i =>
    if i % 2 == 0 then (s"could you send the thing $i", "Request")
    else (s"shall we meet on day $i", "Proposal")
  }

  test("a probe survives the round trip as the same classifier") {
    val fitted = Probe.train(rows)
    val wire = Json.write(Fitted.save(fitted))
    val back = Fitted.load(Json.decode(summon[Schema[Fitted.ProbeModel]])(Json.parseValue(wire))
      .getOrElse(fail("the model did not decode")))
    assertEquals(back.classes, fitted.classes)
    for e <- Seq(vec(1.0, 0.0, 0.3), vec(-1.0, 0.4, 0.3), vec(0.0, 0.0, 0.3)) do
      assertEquals(Probe.score(back, e).map(_.best), Probe.score(fitted, e).map(_.best))
      val a = Probe.score(back, e).map(_.probability).getOrElse(0.0)
      val b = Probe.score(fitted, e).map(_.probability).getOrElse(0.0)
      assertEqualsDouble(a, b, 1e-12, "a loaded probe must not merely agree, it must be identical")
  }

  test("a centroid survives the round trip") {
    val fitted = Centroid.train(rows)
    val back = Fitted.load(Fitted.save(fitted))
    for e <- Seq(vec(1.0, 0.0, 0.3), vec(-1.0, 0.4, 0.3)) do
      assertEquals(Centroid.score(back, e).map(_.best), Centroid.score(fitted, e).map(_.best))
      assertEqualsDouble(Centroid.score(back, e).map(_.similarity).getOrElse(0.0),
        Centroid.score(fitted, e).map(_.similarity).getOrElse(0.0), 1e-12)
  }

  test("a chargram model survives the round trip") {
    val fitted = CharGrams.train(texts, dim = 256, epochs = 20)
    val back = Fitted.load(Fitted.save(fitted))
    for t <- Seq("could you send it", "shall we meet", "unrelated words here") do
      assertEquals(CharGrams.score(back, t).map(_.best), CharGrams.score(fitted, t).map(_.best))
  }

  test("a static table survives the round trip, and takes its splitter back") {
    val words = texts.flatMap((t, _) => Static.units(t)).distinct
    val table = Static.table(words.map(w => w -> vec(w.length * 0.1, 0.2, 0.3)).toMap,
      texts.map(_._1), split = Static.units)
    val back = Fitted.load(Fitted.save(table), Static.units)
    assertEquals(back.size, table.size)
    for t <- texts.take(5).map(_._1) do
      assertEquals(Static.encode(back, t).map(_.toVector), Static.encode(table, t).map(_.toVector))
  }

  test("what a realistic model costs on the wire") {
    // 1024 dimensions and four classes is the shape this programme
    // actually fits; the number belongs in the spec rather than an
    // assurance that bytes are smaller than digits
    val dim = 1024
    val big = (0 until 40).map { i =>
      val a = i % 2 == 0
      (embedding(Array.tabulate(dim)(j => ((if a then 1.0f else -1.0f) * (j % 7) * 0.01f))),
        if a then "A" else "B")
    }
    val fitted = Probe.train(big, epochs = 5)
    val bytes = Json.write(Fitted.save(fitted)).length
    val digits = fitted.w.map(_.mkString(",").length).sum
    println(f"\n[fitted] a ${fitted.classes.length}-class probe over $dim dims: " +
            f"${bytes / 1024}KB as bytes, ${digits / 1024}KB as decimal literals " +
            f"(${digits.toDouble / bytes}%.1fx)")
    assert(bytes < digits, "the byte form must be smaller, or the hand-built schemas are pointless")
  }

  test("the numbers ride as BYTES, not as digits") {
    // the whole reason these schemas are hand-built: a derivation would
    // send 4096 doubles as a page of decimal literals
    val fitted = Probe.train(rows)
    val wire = Json.write(Fitted.save(fitted))
    assert(!wire.contains("0.0,0.0"), "the weights look like a JSON array of numbers")
    assert(wire.length < 40000, s"a 3-dimension probe serialised to ${wire.length} chars")
  }

  property("any probe round-trips to identical predictions") {
    val gen = Gen.listOfN(12, Gen.zip(Gen.choose(-1.0, 1.0), Gen.choose(-1.0, 1.0)))
    forAll(gen) { (pts: List[(Double, Double)]) =>
      val data = pts.zipWithIndex.map { case ((x, y), i) =>
        (vec(x, y, 0.1), if i % 2 == 0 then "A" else "B") }
      val fitted = Probe.train(data, epochs = 30)
      val back = Fitted.load(Fitted.save(fitted))
      pts.forall { (x, y) =>
        Probe.score(back, vec(x, y, 0.1)).map(_.best) ==
          Probe.score(fitted, vec(x, y, 0.1)).map(_.best)
      }
    }
  }
}
