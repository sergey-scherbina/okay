package okay.compress

/** `Mem` for Scala.js: the same signatures as the JVM's, byte by byte
 * (okay-compress-jvm-fast-paths); a `DataView` fast path is its own item */
private[compress] object Mem:
  def i64(b: Array[Byte], at: Int): Long = Le.i64(b, at)
  def i32(b: Array[Byte], at: Int): Int = Le.i32(b, at)
  def put64(b: Array[Byte], at: Int, v: Long): Unit = Le.put64(b, at, v)

  def common(a: Array[Byte], i: Int, b: Array[Byte], j: Int, max: Int): Int =
    var k = 0
    while k < max && a(i + k) == b(j + k) do k += 1
    k

  def copyMatch(dst: Array[Byte], ref: Int, at: Int, len: Int): Unit =
    var k = 0
    while k < len do { dst(at + k) = dst(ref + k); k += 1 }
