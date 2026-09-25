package okay.compress

/** inputs every codec's tests run over, on every platform */
object Samples:
  private def bytes(s: String) = s.getBytes("UTF-8")
  private val rnd = scala.util.Random(42)

  val all: Vector[(String, Array[Byte])] = Vector(
    "empty" -> Array.emptyByteArray,
    "one byte" -> Array[Byte](7),
    "12 bytes" -> bytes("hello, world"),
    "13 bytes" -> bytes("hello, world!"),
    "a run" -> Array.fill(100000)('a'.toByte),
    "a short period" -> Array.tabulate(70000)(i => (i % 3).toByte),
    "text" -> bytes(("the quick brown fox jumps over the lazy dog; чай ☕ " * 2000)),
    "random" -> Array.fill(50000)(rnd.nextInt(256).toByte),
    "random then text" -> (Array.fill(3000)(rnd.nextInt(256).toByte) ++ bytes("abcabcabc" * 3000)),
    "numbers" -> bytes((0 until 20000).map(i => s"$i,${i * 7 % 1000},row$i\n").mkString),
    "far matches" -> {
      val block = Array.fill(40000)(rnd.nextInt(256).toByte)
      block ++ Array.fill(30000)(rnd.nextInt(256).toByte) ++ block
    })

  /** past LZ4's 4 MiB block: several blocks in one frame */
  lazy val big: Array[Byte] = bytes((0 until 400000).map(i => s"line $i of many, value ${i % 97}\n").mkString)
