package okay.parquet

/** Scala.js has no `java.util.zip`: a GZIP page is refused by name */
private[parquet] object Gzip:
  def inflate(b: Array[Byte]): Array[Byte] =
    throw Refused("a GZIP-compressed Parquet page: Scala.js has no inflate; read the file on the JVM or Native")
