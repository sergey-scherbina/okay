package okay.parquet

/** a GZIP page (Hudi's default codec) inflated by the platform —
 * `java.util.zip`, which is DEFLATE on the wire and never ours
 * (specs/own-or-standard.md: platform primitives have nothing to choose
 * against) */
private[parquet] object Gzip:
  def inflate(b: Array[Byte]): Array[Byte] =
    val in = java.util.zip.GZIPInputStream(java.io.ByteArrayInputStream(b))
    try in.readAllBytes() finally in.close()
