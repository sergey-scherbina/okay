package okay2

import okay2.codec.Schema

/** the row-shaped instances okay-codec does not have, imported with
 * `import okay2.sql._` (Scala 3: `import okay.sql.given`); the java.time
 * ones are `okay2.sql.javatime._`, on the JVM only */
package object sql {

  implicit val uuidSchema: Schema[java.util.UUID] = Schema.refine[java.util.UUID, String](
    s => try Right(java.util.UUID.fromString(s)) catch { case _: IllegalArgumentException => Left(s"not a uuid: '$s'") },
    _.toString)

  implicit val decimalSchema: Schema[BigDecimal] = Schema.refine[BigDecimal, String](
    s => try Right(BigDecimal(s)) catch { case _: NumberFormatException => Left(s"not a decimal: '$s'") },
    _.toString)
}
