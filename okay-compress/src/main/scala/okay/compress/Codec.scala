package okay.compress

/**
 * okay-compress (specs/okay-compress.md): LZ4 and ZSTD of our own, pure
 * Scala over `Array[Byte]`, on the JVM, Scala.js and Scala Native, with no
 * dependency. A codec is a pair of functions; input it cannot read —
 * cut short, damaged, or using a feature not implemented — is refused by
 * name ([[Corrupt]]), never answered with wrong bytes.
 */
trait Codec:
  def name: String
  def compress(bytes: Array[Byte]): Array[Byte]
  def decompress(bytes: Array[Byte]): Array[Byte]

/** input a codec will not read, and why */
final class Corrupt(message: String) extends IllegalStateException(message)

/** little-endian reads and writes over byte arrays, shared by the codecs */
private[compress] object Le:
  inline def u8(b: Array[Byte], at: Int): Int = b(at) & 0xff
  inline def u16(b: Array[Byte], at: Int): Int = (b(at) & 0xff) | (b(at + 1) & 0xff) << 8
  inline def i32(b: Array[Byte], at: Int): Int =
    (b(at) & 0xff) | (b(at + 1) & 0xff) << 8 | (b(at + 2) & 0xff) << 16 | (b(at + 3) & 0xff) << 24
  inline def i64(b: Array[Byte], at: Int): Long =
    (i32(b, at).toLong & 0xffffffffL) | (i32(b, at + 4).toLong << 32)
  inline def put32(b: Array[Byte], at: Int, v: Int): Unit =
    b(at) = v.toByte; b(at + 1) = (v >>> 8).toByte; b(at + 2) = (v >>> 16).toByte; b(at + 3) = (v >>> 24).toByte
  inline def put64(b: Array[Byte], at: Int, v: Long): Unit =
    put32(b, at, v.toInt); put32(b, at + 4, (v >>> 32).toInt)

/** a growable output: codecs write into it without a copy per append */
private[compress] final class Out(initial: Int):
  var buf: Array[Byte] = new Array[Byte](math.max(16, initial))
  var n: Int = 0
  def room(k: Int): Unit =
    if n + k > buf.length then buf = java.util.Arrays.copyOf(buf, math.max(buf.length * 2, n + k))
  def byte(v: Int): Unit = { room(1); buf(n) = v.toByte; n += 1 }
  def int32(v: Int): Unit = { room(4); Le.put32(buf, n, v); n += 4 }
  def int64(v: Long): Unit = { room(8); Le.put64(buf, n, v); n += 8 }
  def bytes(b: Array[Byte], from: Int, len: Int): Unit = { room(len); System.arraycopy(b, from, buf, n, len); n += len }
  def result(): Array[Byte] = if n == buf.length then buf else java.util.Arrays.copyOf(buf, n)
