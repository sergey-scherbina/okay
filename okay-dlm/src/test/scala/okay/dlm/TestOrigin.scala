package okay.dlm

import munit.FunSuite
import java.nio.file.{Files, Path}

/** a table named by what it was built from — one test per behavior
 * line of specs/dlm-learning.md §11 */
class TestOrigin extends FunSuite:
  import Ledger.Entry

  val rows = Vector("need" -> "нужен сантехник", "need" -> "ищу электрика", "offer" -> "умею чинить")
  val ours = Embedder.hashing(64)
  /** the same encoder whose numbers came out a hair apart — what the
   * same model gives on another processor */
  val jittered = Embedder.of(ours.name, ours.dim, t => okay.rag.embedding(ours(t).toArray.map(_ * 1.0000001f + 1e-7f)),
    ours.fingerprint)

  val tmp = FunFixture[Path](_ => Files.createTempDirectory("origin"), d =>
    Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p): Unit))

  test("the same rows through the same encoder name the same origin whatever the numbers; any change of input names another") {
    val a = Exemplars.compile(rows)(using ours)
    val b = Exemplars.compile(rows)(using jittered)
    assertNotEquals(a.hash, b.hash, "the numbers moved, so the bytes' hash did")
    assertEquals(a.origin, b.origin)
    assert(a.origin.matches("[0-9a-f]{32}"), a.origin)
    val others = Vector(
      rows :+ ("offer" -> "могу помочь"),                     // a row added
      rows.dropRight(1),                                      // a row dropped
      rows.updated(2, "need" -> "умею чинить"),               // relabelled
      rows.reverse)                                           // reordered
    others.foreach(r => assertNotEquals(Exemplars.compile(r)(using ours).origin, a.origin, r.toString))
    assertNotEquals(Exemplars.compile(rows)(using Embedder.hashing(32)).origin, a.origin, "another encoder")
    assertNotEquals(Exemplars.compile(rows, ours(_), ours.name, "other-tensors").origin, a.origin, "another fingerprint")
    // a pair is not two words: («a b», «c») and («a», «b c») are different corpora
    assertNotEquals(Exemplars.corpusOf(Vector("a b" -> "c")), Exemplars.corpusOf(Vector("a" -> "b c")))
  }

  tmp.test("provenance travels through the checkpoint, the JSON and `stored`; a table from before reads with none") { dir =>
    val t = Exemplars.compile(rows)(using ours)
    val json = dir.resolve("heads.vec.json")
    Exemplars.write(json, t)
    val back = Exemplars.read(json).toOption.get
    assertEquals((back.provenance, back.origin), (t.provenance, t.origin))
    assertEquals(Exemplars.parse(Exemplars.print(t)).map(_.origin), Right(t.origin))
    assertEquals(t.stored().origin, t.origin)
    assertEquals(Checkpoint.meta(Checkpoint.binaryOf(json)).map(_.get("origin")), Right(Some(t.origin)))
    // written before this stage: no corpus, no fingerprint, no origin — read, not refused
    val old = Exemplars(t.encoder, t.dim, t.rows)
    Exemplars.write(dir.resolve("old.vec.json"), old)
    val oldBack = Exemplars.read(dir.resolve("old.vec.json")).toOption.get
    assertEquals((oldBack.provenance, oldBack.origin), (Exemplars.Provenance.unknown, ""))
  }

  test("`agrees` gives the lowest cosine for two builds of one table, and names another encoder, other labels, a row below the bar") {
    val a = Exemplars.compile(rows)(using ours)
    val b = Exemplars.compile(rows)(using jittered)
    assert(Exemplars.agrees(a, b).exists(_ > 0.9999), Exemplars.agrees(a, b).toString)
    assertEquals(Exemplars.agrees(a, a), Right(1.0))
    assert(Exemplars.agrees(a, Exemplars.compile(rows)(using Embedder.hashing(32))).left.exists(_.contains("encoders differ")))
    assert(Exemplars.agrees(a, Exemplars.compile(rows.updated(2, "need" -> "умею чинить"))(using ours))
      .left.exists(_.contains("row 2")))
    assert(Exemplars.agrees(a, Exemplars.compile(rows.dropRight(1))(using ours)).left.exists(_.contains("3 rows against 2")))
    val moved = a.copy(rows = a.rows.updated(1, a.rows(1).copy(vec = a.rows(0).vec)))
    val why = Exemplars.agrees(a, moved)
    assert(why.left.exists(w => w.contains("row 1") && w.contains("«need»")), why.toString)
  }

  test("`accept` refuses another encoder by name and another fingerprint when both carry one, and takes a table with none") {
    val t = Exemplars.compile(rows)(using ours)
    assertEquals(Exemplars.accept(t, ours), Right(t))
    assert(Exemplars.accept(t, Embedder.hashing(32)).left.exists(_.contains("refused")))
    val sameNameOtherNumbers = Embedder.of(ours.name, ours.dim, ours(_), "int8-file")
    assert(Exemplars.accept(t, sameNameOtherNumbers).left.exists(_.contains("the same name, other numbers")))
    // a remote encoder knows no fingerprint: its name decides
    assertEquals(Exemplars.accept(t, Embedder.of(ours.name, ours.dim, ours(_))), Right(t))
    // a table from before this stage knows none either
    val old = Exemplars(t.encoder, t.dim, t.rows)
    assertEquals(Exemplars.accept(old, sameNameOtherNumbers), Right(old))
  }

  test("`hashing` carries a fingerprint; `Embedder.of` carries the one it is given") {
    assertEquals(Embedder.hashing(64).fingerprint, "okay.rag.Vectors.hashing/1/64")
    assertNotEquals(Embedder.hashing(32).fingerprint, Embedder.hashing(64).fingerprint)
    assertEquals(Embedder.of("m", 3, _ => okay.rag.embedding(Array(1f, 0f, 0f)), "tensors:abc").fingerprint, "tensors:abc")
    assertEquals(Embedder.of("m", 3, _ => okay.rag.embedding(Array(1f, 0f, 0f))).fingerprint, "")
    assertEquals(Exemplars.compile(rows)(using ours).provenance.fingerprint, ours.fingerprint)
  }

  tmp.test("`Rebuilt` carries the origin on the wire, an older entry reads with none, and the shelf names what it keeps by origin") { dir =>
    val t = Exemplars.compile(rows)(using ours)
    val e = Entry.Rebuilt("intents", t.encoder, None, t.stored().hash, "corpus", 1L, "ci", t.origin)
    assertEquals(Ledger.parse(Ledger.line(e)), Vector(e))
    val older = """{"entry":"rebuilt","artifact":"intents","encoder":"e","after":"a","corpus":"c","at":1,"by":"ci"}"""
    assertEquals(Ledger.parse(older), Vector(Entry.Rebuilt("intents", "e", None, "a", "c", 1L, "ci")))
    for shelf <- Vector(Shelf.directory(dir), Shelf.memory()) do
      val k = shelf.put("intents", t, 5L)
      assertEquals(k.origin, t.origin)
      assertEquals(shelf.kept("intents").map(_.origin), Vector(t.origin))
      assertEquals(shelf.get("intents", k.hash).map(_.origin), Right(t.origin))
  }
