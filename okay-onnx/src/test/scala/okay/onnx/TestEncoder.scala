package okay.onnx

import okay.rag.Vectors
import java.nio.file.{Files, Path}

/**
 * specs/intent-spans.md — the encoder. A model directory is named by
 * OKAY_ONNX_MODEL; without one every test here SKIPS and says so,
 * because a 120 MB file is not a thing `sbt test` may fetch.
 */
class TestEncoder extends munit.FunSuite {

  private val modelDir: Option[Path] =
    sys.env.get("OKAY_ONNX_MODEL").map(Path.of(_)).filter(p => Files.exists(p.resolve("model.onnx")))

  private def withEncoder(body: Encoder => Unit): Unit = modelDir match
    case None => assume(false, "OKAY_ONNX_MODEL names no model directory — skipped")
    case Some(dir) =>
      val enc = Encoder(dir)
      try body(enc) finally enc.close()

  test("the pooled vector is the mean of every position, unit length — the production embedding") {
    withEncoder { enc =>
      val e = enc.encode("нужен сантехник во Вроцлаве")
      assertEquals(e.pooled.length, enc.dim)
      assertEqualsDouble(Vectors.cosine(e.pooled, Vectors.normalize(e.pooled)).toDouble, 1.0, 1e-5)
      val norm = math.sqrt(e.pooled.map(x => x.toDouble * x).sum)
      assertEqualsDouble(norm, 1.0, 1e-4)
      // and the same text encodes to the same vector: no state between calls
      assertEquals(enc.embed("нужен сантехник во Вроцлаве"), e.pooled)
    }
  }

  test("tokens carry the text's own characters, and the specials are gone") {
    withEncoder { enc =>
      val text = "что интересного во Вроцлаве"
      val toks = enc.encode(text).tokens
      assert(toks.nonEmpty)
      assert(toks.forall(t => t.start >= 0 && t.end <= text.length && t.start < t.end), toks.map(t => (t.text, t.start, t.end)))
      assert(!toks.exists(t => Encoder.specials(t.text)))
      // every non-space character of the text is under some token
      val covered = toks.flatMap(t => t.start until t.end).toSet
      assert(text.indices.filterNot(i => text(i).isWhitespace).forall(covered), "a character no token covers")
      assert(toks.forall(_.vector.length == enc.dim))
    }
  }

  test("a token inside a sentence is not the token alone — the encoder is contextual") {
    withEncoder { enc =>
      val alone = enc.encode("Вроцлаве").tokens.map(_.vector)
      val inside = enc.encode("что интересного сегодня вечером во Вроцлаве").tokens.takeRight(alone.length).map(_.vector)
      val agreement = alone.zip(inside).map((a, b) => Vectors.cosine(a, b)).sum / alone.length
      assert(agreement < 0.95, s"alone and in context agree at $agreement — then prototypes in context would be pointless")
      assert(agreement > 0.3, s"alone and in context agree only at $agreement — the pieces do not line up")
    }
  }

  test("okay-rag's effect answers the same vectors as the plain function") {
    withEncoder { enc =>
      given okay.Handler[okay.rag.Embed] = enc.handler
      import okay.given
      val via = okay.rag.embed(Seq("окей")).runWith
      assertEquals(via.head, enc.embed("окей"))
    }
  }
}
