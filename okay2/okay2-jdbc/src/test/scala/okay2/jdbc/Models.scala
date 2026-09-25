package okay2.jdbc

// the rows the suites read, at the top level: a case class nested in a
// suite class carries an outer reference its pattern cannot check

final case class Customer(id: Long, userName: String, age: Option[Int], balance: Double, active: Boolean,
                          avatar: Option[Array[Byte]])
/** the drifted view of the same table: age as non-Option */
final case class Strict(id: Long, age: Int)
final case class Name(s: String)
final case class Named(id: Long, userName: Name)
final case class Short5(s: String)
final case class R(id: Long, userName: Short5)
final case class Filter(minBalance: Double, active: Boolean)
final case class NewRow(id: Long, userName: String, balance: Double, active: Boolean)
final case class Count(n: Long)
final case class Big(n: Int, label: String)
final case class Tagged(id: Int, tags: Vector[String], nums: Option[List[Option[Int]]])
final case class Ledger(id: Int, amount: BigDecimal, at: java.time.Instant)
final case class LedgerText(id: Int, at: String)
final case class Rounded(id: Int, amount: Double)
final case class Stamp(id: Int, at: java.time.Instant, plain: java.time.Instant, d: java.time.LocalDate,
                       t: java.time.LocalTime, ref: java.util.UUID, note: Option[String])
final case class QCustomer(id: Long, userName: String, age: Option[Int], balance: Double, active: Boolean)
final case class SqliteStamp(id: Long, at: java.time.Instant, d: java.time.LocalDate)
