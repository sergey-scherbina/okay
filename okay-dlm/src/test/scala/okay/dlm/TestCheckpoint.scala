package okay.dlm

import munit.FunSuite
import java.nio.file.Files

class TestCheckpoint extends FunSuite:

  val labels = Vector("need", "offer", "что́-то с ударением", "日本語")
  val vecs = Vector(Array(0.5f, -0.25f, 1e-3f), Array(1f, 0f, 0f), Array(-1f, 2f, 3f), Array(0.1f, 0.2f, 0.3f))

  test("the bytes read back: labels in UTF-8, floats exactly, metadata as written, weights when present") {
    val b = Checkpoint.bytes("enc", 3, labels, vecs, Map("offset" -> "42"), weights = Array(1.5, 2.5))
    val l = Checkpoint.of(b, Some(("enc", 3)), "t").toOption.get
    assertEquals(l.labels, labels)
    assertEquals(l.vecs.map(_.toVector), vecs.map(_.toVector))
    assertEquals(l.meta("offset"), "42")
    assertEquals(l.meta("count"), "4")
    assertEquals(l.weights.toVector, Vector(1.5, 2.5))
  }

  test("F16 halves the bytes and the header's dtype decides how they are read") {
    val f32 = Checkpoint.bytes("enc", 3, labels, vecs)
    val f16 = Checkpoint.bytes("enc", 3, labels, vecs, f16 = true)
    assert(f16.limit() < f32.limit())
    val l = Checkpoint.of(f16, None, "t").toOption.get
    for (a, b) <- l.vecs.zip(vecs); (x, y) <- a.zip(b) do
      assert(math.abs(x - y) <= math.abs(y) * 1e-3 + 1e-6, s"$x vs $y")
  }

  test("half precision: round trip, ties to even, the extremes") {
    for f <- Vector(0f, -0f, 1f, -1f, 0.5f, 65504f, 6.1e-5f, 1e-8f, Float.PositiveInfinity) do
      val h = Checkpoint.halfToFloat(Checkpoint.floatToHalf(f))
      if f == 0f then assertEquals(h, f)
      else if f.isInfinite then assert(h.isInfinite)
      else if math.abs(f) < 6e-5f then assert(math.abs(h - f) <= 6e-8f, s"$f -> $h")
      else assert(math.abs(h - f) <= math.abs(f) * 1e-3, s"$f -> $h")
    assert(Checkpoint.halfToFloat(Checkpoint.floatToHalf(Float.NaN)).isNaN)
    assert(Checkpoint.halfToFloat(Checkpoint.floatToHalf(1e6f)).isInfinite)
  }

  test("the bytes are a pure function of the numbers") {
    val a = Checkpoint.bytes("enc", 3, labels, vecs)
    val b = Checkpoint.bytes("enc", 3, labels, vecs)
    assertEquals(a, b)
  }

  test("a checkpoint made by another encoder, of another width or format is refused by name, not read") {
    val b = Checkpoint.bytes("enc", 3, labels, vecs)
    assert(Checkpoint.of(b.duplicate(), Some(("other", 3)), "t").left.exists(_.contains("made by «enc»")))
    assert(Checkpoint.of(b.duplicate(), Some(("enc", 4)), "t").left.exists(_.contains("dim 3")))
    val broken = java.nio.ByteBuffer.wrap(Array[Byte](1, 2, 3))
    assert(Checkpoint.of(broken, None, "t").isLeft)
    assert(Checkpoint.absent("x: not in the image"))
    assert(!Checkpoint.absent("x: format 0, this reader is 1"))
  }

  test("written whole, moved into place, mapped back") {
    val dir = Files.createTempDirectory("dlm")
    val path = dir.resolve("heads/acts.safetensors")
    Checkpoint.write(path, "enc", 3, labels, vecs, f16 = true)
    assert(Files.exists(path) && !Files.exists(path.resolveSibling("acts.safetensors.part")))
    val l = Checkpoint.read(path, Some(("enc", 3))).toOption.get
    assertEquals(l.labels, labels)
    assert(Checkpoint.read(dir.resolve("none.safetensors")).left.exists(Checkpoint.absent))
    assertEquals(Checkpoint.binaryOf(dir.resolve("acts.vec.json")), dir.resolve("acts.safetensors"))
    assertEquals(Checkpoint.binaryOf("/lang.vec.json"), "/lang.safetensors")
  }
