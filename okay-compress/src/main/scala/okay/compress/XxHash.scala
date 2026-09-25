package okay.compress

/**
 * xxHash (Yann Collet): XXH32 is the LZ4 frame's checksum, XXH64 the ZSTD
 * frame's (the low 32 bits). The reference algorithm, over a slice of a
 * byte array, little-endian lanes.
 */
object XxHash:
  private final val P32_1 = 0x9e3779b1
  private final val P32_2 = 0x85ebca77
  private final val P32_3 = 0xc2b2ae3d
  private final val P32_4 = 0x27d4eb2f
  private final val P32_5 = 0x165667b1

  def xxh32(b: Array[Byte], from: Int, len: Int, seed: Int = 0): Int =
    val end = from + len
    var i = from
    var h =
      if len >= 16 then
        var v1 = seed + P32_1 + P32_2
        var v2 = seed + P32_2
        var v3 = seed
        var v4 = seed - P32_1
        val limit = end - 16
        while i <= limit do
          v1 = Integer.rotateLeft(v1 + Le.i32(b, i) * P32_2, 13) * P32_1
          v2 = Integer.rotateLeft(v2 + Le.i32(b, i + 4) * P32_2, 13) * P32_1
          v3 = Integer.rotateLeft(v3 + Le.i32(b, i + 8) * P32_2, 13) * P32_1
          v4 = Integer.rotateLeft(v4 + Le.i32(b, i + 12) * P32_2, 13) * P32_1
          i += 16
        Integer.rotateLeft(v1, 1) + Integer.rotateLeft(v2, 7) + Integer.rotateLeft(v3, 12) + Integer.rotateLeft(v4, 18)
      else seed + P32_5
    h += len
    while i + 4 <= end do
      h = Integer.rotateLeft(h + Le.i32(b, i) * P32_3, 17) * P32_4
      i += 4
    while i < end do
      h = Integer.rotateLeft(h + (b(i) & 0xff) * P32_5, 11) * P32_1
      i += 1
    h ^= h >>> 15; h *= P32_2; h ^= h >>> 13; h *= P32_3; h ^= h >>> 16
    h

  private final val P64_1 = 0x9e3779b185ebca87L
  private final val P64_2 = 0xc2b2ae3d27d4eb4fL
  private final val P64_3 = 0x165667b19e3779f9L
  private final val P64_4 = 0x85ebca77c2b2ae63L
  private final val P64_5 = 0x27d4eb2f165667c5L

  private inline def round(acc: Long, lane: Long): Long = java.lang.Long.rotateLeft(acc + lane * P64_2, 31) * P64_1
  private inline def merge(acc: Long, v: Long): Long = (acc ^ round(0L, v)) * P64_1 + P64_4

  def xxh64(b: Array[Byte], from: Int, len: Int, seed: Long = 0L): Long =
    val end = from + len
    var i = from
    var h =
      if len >= 32 then
        var v1 = seed + P64_1 + P64_2
        var v2 = seed + P64_2
        var v3 = seed
        var v4 = seed - P64_1
        val limit = end - 32
        while i <= limit do
          v1 = round(v1, Le.i64(b, i)); v2 = round(v2, Le.i64(b, i + 8))
          v3 = round(v3, Le.i64(b, i + 16)); v4 = round(v4, Le.i64(b, i + 24))
          i += 32
        var acc = java.lang.Long.rotateLeft(v1, 1) + java.lang.Long.rotateLeft(v2, 7) +
          java.lang.Long.rotateLeft(v3, 12) + java.lang.Long.rotateLeft(v4, 18)
        acc = merge(acc, v1); acc = merge(acc, v2); acc = merge(acc, v3); merge(acc, v4)
      else seed + P64_5
    h += len.toLong
    while i + 8 <= end do
      h ^= round(0L, Le.i64(b, i))
      h = java.lang.Long.rotateLeft(h, 27) * P64_1 + P64_4
      i += 8
    if i + 4 <= end then
      h ^= (Le.i32(b, i).toLong & 0xffffffffL) * P64_1
      h = java.lang.Long.rotateLeft(h, 23) * P64_2 + P64_3
      i += 4
    while i < end do
      h ^= (b(i) & 0xffL) * P64_5
      h = java.lang.Long.rotateLeft(h, 11) * P64_1
      i += 1
    h ^= h >>> 33; h *= P64_2; h ^= h >>> 29; h *= P64_3; h ^= h >>> 32
    h
