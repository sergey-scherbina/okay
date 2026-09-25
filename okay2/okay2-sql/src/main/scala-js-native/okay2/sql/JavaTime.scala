package okay2.sql

/** no java.time instances off the JVM (okay-sql's scala-js/scala-native
 * JavaTime): `Temporal` reads and writes the numbers */
object javatime {
  private[sql] val known: Vector[Typed.Known[_]] = Vector.empty
}
