package okay.crypto

/**
 * Keccak-256 — the Ethereum hash (keccak-pure, 2026-09-25).
 *
 * The ORIGINAL Keccak as Ethereum froze it, NOT NIST SHA3-256: the two
 * differ by ONE byte, the domain-separation pad (`0x01` against `0x06`),
 * so a JDK `MessageDigest.getInstance("SHA3-256")` compiles, runs, and
 * answers the wrong digest for every input. Pure Scala over `Long`, no
 * dependency and no platform API, so it is the same on the JVM and JS.
 * Ported from scalascript's `crypto/spi/shared` by okay-watch, where it
 * checked EIP-55; one implementation here now for okay-x402-evm and
 * okay-watch alike. Rate 1088 bits (136 bytes), capacity 512, 256-bit out.
 */
object Keccak256:

  /** Keccak-256 digest (32 bytes) of `data`. */
  def hash(data: Array[Byte]): Array[Byte] =
    val rate  = 136                       // 1088-bit rate = 17 lanes
    val st    = new Array[Long](25)       // 5x5 lanes of the 1600-bit state
    val len   = data.length
    var off   = 0
    while len - off >= rate do
      absorb(st, data, off, rate)
      off += rate
    // Final block: remaining bytes, zero-padded, with Keccak pad10*1 (0x01 … 0x80).
    val block = new Array[Byte](rate)
    val rem   = len - off
    System.arraycopy(data, off, block, 0, rem)
    block(rem)      = (block(rem) ^ 0x01).toByte
    block(rate - 1) = (block(rate - 1) ^ 0x80.toByte).toByte
    absorb(st, block, 0, rate)
    // Squeeze the first 32 output bytes (little-endian lanes; one block covers 256 bits < rate).
    val out = new Array[Byte](32)
    var i = 0
    while i < 32 do
      out(i) = ((st(i >>> 3) >>> (8 * (i & 7))) & 0xffL).toByte
      i += 1
    out

  /** XOR `rate` little-endian bytes of `src` (from `off`) into the state lanes, then permute. */
  private def absorb(st: Array[Long], src: Array[Byte], off: Int, rate: Int): Unit =
    var k = 0
    while k < rate do
      st(k >>> 3) ^= (src(off + k) & 0xffL) << (8 * (k & 7))
      k += 1
    keccakF1600(st)

  private inline def rotl(x: Long, n: Int): Long =
    if n == 0 then x else (x << n) | (x >>> (64 - n))

  // Round constants ι, rotation offsets ρ, and the π lane-permutation — the canonical
  // Keccak-f[1600] tables (matching the reference "tiny_sha3" compact implementation).
  private val RC: Array[Long] = Array(
    0x0000000000000001L, 0x0000000000008082L, 0x800000000000808aL, 0x8000000080008000L,
    0x000000000000808bL, 0x0000000080000001L, 0x8000000080008081L, 0x8000000000008009L,
    0x000000000000008aL, 0x0000000000000088L, 0x0000000080008009L, 0x000000008000000aL,
    0x000000008000808bL, 0x800000000000008bL, 0x8000000000008089L, 0x8000000000008003L,
    0x8000000000008002L, 0x8000000000000080L, 0x000000000000800aL, 0x800000008000000aL,
    0x8000000080008081L, 0x8000000000008080L, 0x0000000080000001L, 0x8000000080008008L,
  )
  private val RHO: Array[Int] = Array(
    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 2, 14,
    27, 41, 56, 8, 25, 43, 62, 18, 39, 61, 20, 44,
  )
  private val PILN: Array[Int] = Array(
    10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9, 6, 1,
  )

  /** The Keccak-f[1600] permutation over 25 lanes (24 rounds: θ, ρ, π, χ, ι). */
  private def keccakF1600(a: Array[Long]): Unit =
    val bc = new Array[Long](5)
    var r = 0
    while r < 24 do
      // θ (theta)
      var i = 0
      while i < 5 do
        bc(i) = a(i) ^ a(i + 5) ^ a(i + 10) ^ a(i + 15) ^ a(i + 20)
        i += 1
      i = 0
      while i < 5 do
        val t = bc((i + 4) % 5) ^ rotl(bc((i + 1) % 5), 1)
        var j = 0
        while j < 25 do { a(j + i) ^= t; j += 5 }
        i += 1
      // ρ (rho) + π (pi)
      var t = a(1)
      i = 0
      while i < 24 do
        val j = PILN(i)
        bc(0) = a(j)
        a(j)  = rotl(t, RHO(i))
        t     = bc(0)
        i += 1
      // χ (chi)
      var jj = 0
      while jj < 25 do
        i = 0
        while i < 5 do { bc(i) = a(jj + i); i += 1 }
        i = 0
        while i < 5 do { a(jj + i) ^= (~bc((i + 1) % 5)) & bc((i + 2) % 5); i += 1 }
        jj += 5
      // ι (iota)
      a(0) ^= RC(r)
      r += 1
