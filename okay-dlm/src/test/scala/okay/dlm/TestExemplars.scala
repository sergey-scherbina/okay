package okay.dlm

import munit.FunSuite
import java.nio.file.Files

class TestExemplars extends FunSuite:

  val embed = okay.rag.Vectors.hashing(32)
  val rows = Vector("yes" -> "да", "yes" -> "конечно", "no" -> "нет", "tell" -> "а что там?")
  val ex = Exemplars.compile(rows, embed, "hashing-32")

  test("compiled: a label and a vector per phrase, the encoder stamped in") {
    assertEquals(ex.dim, 32)
    assertEquals(ex.labels, Vector("yes", "no", "tell"))
    assertEquals(ex.rows.map(_.phrase), rows.map(_._2))
    assertEquals(ex.labelled.length, 4)
  }

  test("the JSON artifact carries no phrase and reads back the same numbers — under `label` or the older `intent`") {
    val printed = Exemplars.print(ex)
    assert(!printed.contains("конечно"))
    val back = Exemplars.parse(printed).toOption.get
    assertEquals(back.rows.map(e => (e.label, e.vec)), ex.rows.map(e => (e.label, e.vec)))
    val older = printed.replace("\"label\"", "\"intent\"")
    assertEquals(Exemplars.parse(older).toOption.get.labels, ex.labels)
    // a contradicted dimension is refused, naming the index
    val wrong = """{"model":"m","dim":2,"entries":[{"label":"a","vec":[1,2]},{"label":"b","vec":[1]}]}"""
    assert(Exemplars.parse(wrong).left.exists(_.contains("index 1")))
  }

  test("write puts both artifacts beside each other and read takes the binary first") {
    val dir = Files.createTempDirectory("dlm")
    val json = dir.resolve("answers.vec.json")
    Exemplars.write(json, ex, Map("corpus" -> "abc"))
    assert(Files.exists(json) && Files.exists(dir.resolve("answers.safetensors")))
    val back = Exemplars.read(json, Some(("hashing-32", 32))).toOption.get
    assertEquals(back.labels, ex.labels)
    assertEquals(back.encoder, "hashing-32")
    // …and falls back to the JSON, saying so, when the binary disagrees
    var warned = Vector.empty[String]
    val viaJson = Exemplars.read(json, Some(("other", 32)), warned :+= _).toOption.get
    assertEquals(viaJson.labels, ex.labels)
    assert(warned.exists(_.contains("refused")), warned.toString)
    // silently when there is no binary at all
    Files.delete(dir.resolve("answers.safetensors"))
    warned = Vector.empty
    assert(Exemplars.read(json, None, warned :+= _).isRight)
    assertEquals(warned, Vector.empty)
    assert(Exemplars.read(dir.resolve("none.vec.json")).isLeft)
  }

  test("the guard refuses to overwrite an artifact of another encoder unless the change is declared") {
    val dir = Files.createTempDirectory("dlm")
    val json = dir.resolve("x.vec.json")
    assertEquals(Exemplars.guard(json, "b"), Right(()))       // nothing there yet
    Files.writeString(json, Exemplars.print(ex))
    assert(Exemplars.guard(json, "other").isLeft)
    assertEquals(Exemplars.guard(json, "hashing-32"), Right(()))
    assertEquals(Exemplars.guard(json, "other", force = true), Right(()))
  }

  test("a resource the image does not carry is None, which is a supported deployment") {
    assertEquals(Exemplars.resource("/no-such.vec.json"), None)
  }
