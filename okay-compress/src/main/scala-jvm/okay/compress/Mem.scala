package okay.compress

import java.lang.invoke.{MethodHandles, VarHandle}
import java.nio.ByteOrder

/**
 * Eight- and four-byte little-endian access to a byte array, THE JVM's
 * way: a `VarHandle` view, which the JIT compiles to one load or store
 * (okay-compress-jvm-fast-paths). Scala.js and Native have their own
 * `Mem` with the same signatures, byte by byte, so the codecs stay one
 * source.
 */
private[compress] object Mem:
  private val Long64: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Long]], ByteOrder.LITTLE_ENDIAN)
  private val Int32: VarHandle = MethodHandles.byteArrayViewVarHandle(classOf[Array[Int]], ByteOrder.LITTLE_ENDIAN)

  def i64(b: Array[Byte], at: Int): Long = Long64.get(b, at): Long
  def i32(b: Array[Byte], at: Int): Int = Int32.get(b, at): Int
  def put64(b: Array[Byte], at: Int, v: Long): Unit = Long64.set(b, at, v): Unit

  /** how many leading bytes of `a[i..]` and `b[j..]` agree, at most `max` */
  def common(a: Array[Byte], i: Int, b: Array[Byte], j: Int, max: Int): Int =
    var k = 0
    while k + 8 <= max do
      val x = i64(a, i + k) ^ i64(b, j + k)
      if x != 0 then return k + (java.lang.Long.numberOfTrailingZeros(x) >>> 3)
      k += 8
    while k < max && a(i + k) == b(j + k) do k += 1
    k

  /** `len` bytes from `src[from]` to `dst[at]`, where a match may overlap
   * its own output: eight at a time when the distance allows it */
  def copyMatch(dst: Array[Byte], ref: Int, at: Int, len: Int): Unit =
    if at - ref >= 8 then
      var k = 0
      while k + 8 <= len do { put64(dst, at + k, i64(dst, ref + k)); k += 8 }
      while k < len do { dst(at + k) = dst(ref + k); k += 1 }
    else
      var k = 0
      while k < len do { dst(at + k) = dst(ref + k); k += 1 }
