package fixture

/** a Scala 3 lazy val: the compiler's VarHandle idiom, not a reach */
object Lazy:
  lazy val table: Map[Int, String] = (1 to 3).map(i => i -> i.toString).toMap
  def get(i: Int): Option[String] = table.get(i)
