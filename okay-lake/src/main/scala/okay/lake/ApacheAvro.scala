package okay.lake

import scala.jdk.CollectionConverters.*

/**
 * APACHE AVRO BEHIND THE FACADE (specs/own-or-standard.md):
 * `import okay.lake.ApacheAvro.given`, over an OPTIONAL dependency
 * (org.apache.avro:avro). The same values as `OkayAvro`; without the jar
 * the first use is refused by name ([[ApacheAvro.missing]]).
 */
object ApacheAvro extends AvroReader:
  given apacheAvro: AvroReader = this

  def name = "apache-avro"

  def missing(className: String = "org.apache.avro.file.DataFileReader"): Option[String] =
    try { Class.forName(className, false, getClass.getClassLoader); None }
    catch case _: ClassNotFoundException => Some(
      s"okay.lake.ApacheAvro needs org.apache.avro:avro, an optional dependency of okay-lake ($className is not on the classpath): " +
        "add org.apache.avro:avro:1.12.1 — or use okay.lake.OkayAvro, the default, which needs nothing")

  private lazy val ready: Unit = missing().foreach(why => throw IllegalStateException(why))

  def records(bytes: Array[Byte]): Vector[Any] =
    ready
    val r = org.apache.avro.file.DataFileReader(
      org.apache.avro.file.SeekableByteArrayInput(bytes),
      org.apache.avro.generic.GenericDatumReader[Any]())
    try r.iterator().asScala.map(plain(_, 0)).toVector
    finally r.close()

  /** the library's value as the facade's; bounded by the schema's depth,
   * which Avro's own reader has walked to produce it */
  private def plain(v: Any, depth: Int): Any =
    if depth > OkayAvro.MaxDepth then throw AvroRefused(s"Avro data nested deeper than ${OkayAvro.MaxDepth}")
    v match
      case null => null
      case r: org.apache.avro.generic.GenericRecord =>
        r.getSchema.getFields.asScala.toVector.map(f => f.name -> plain(r.get(f.pos), depth + 1))
      case s: org.apache.avro.util.Utf8 => s.toString
      case s: String => s
      case i: java.lang.Integer => i.longValue
      case l: java.lang.Long => l.longValue
      case b: java.nio.ByteBuffer => { val a = new Array[Byte](b.remaining()); b.duplicate().get(a); a }
      case f: org.apache.avro.generic.GenericFixed => f.bytes.clone()
      case e: org.apache.avro.generic.GenericEnumSymbol[?] => e.toString
      case m: java.util.Map[?, ?] => m.asScala.map((k, x) => k.toString -> plain(x, depth + 1)).toMap
      case xs: java.util.Collection[?] => xs.asScala.toVector.map(plain(_, depth + 1))
      case other => other
