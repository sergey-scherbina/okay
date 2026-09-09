package okay.sql

/** no java.time on this platform: the temporal columns are read into
 * String fields (ISO 8601 text, Temporal.scala) or through the raw
 * SqlValue cases; the JVM leg adds Instant/LocalDate/LocalTime */
private[sql] object JavaTime:
  val known: Vector[Typed.Known[?]] = Vector.empty
