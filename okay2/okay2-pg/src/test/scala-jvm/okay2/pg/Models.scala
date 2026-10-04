package okay2.pg

import okay2.codec.Schema
import okay2.sql._
import okay2.sql.javatime._

// the rows the suites read, at the top level: a case class nested in a
// suite class carries an outer reference its pattern cannot check
final case class Customer(id: Long, userName: String, age: Option[Int], balance: Double, active: Boolean,
                          avatar: Option[Array[Byte]])
final case class Big(n: Int, label: String)
final case class NewRow(id: Long, userName: String, balance: Double, active: Boolean)
final case class Stamp(id: Int, at: java.time.Instant, plain: java.time.Instant, d: java.time.LocalDate,
                       t: java.time.LocalTime, ref: java.util.UUID, doc: String, ats: Vector[java.time.Instant])
final case class AccCustomer(id: Long, userName: String, age: Option[Int], balance: Double, active: Boolean)
final case class Strict(id: Long, age: Int)
final case class Label(label: Option[String])
final case class Addr(street: String, zip: Option[Int], active: Boolean)
final case class Person(id: Int, nums: Vector[Int], home: Addr, moves: Vector[Addr], prev: Option[Addr])
final case class Wrong(id: Int, home: Vector[Int])
// a whole-row column has no table column behind it, so pg cannot promise
// it not null (an outer join nulls the whole row): Option
final case class Wrap(p: Option[Person])
final case class WrapStrict(p: Person)
final case class In(nums: Vector[Int], home: Addr)
final case class Out(nums: Vector[Int], home: Addr)
final case class Ledger(id: Int, amount: BigDecimal, ref: String, doc: String, at: String)

object Models {
  implicit val customer: Schema[Customer] = Schema.derived
  implicit val big: Schema[Big] = Schema.derived
  implicit val newRow: Schema[NewRow] = Schema.derived
  implicit val stamp: Schema[Stamp] = Schema.derived
  implicit val accCustomer: Schema[AccCustomer] = Schema.derived
  implicit val strict: Schema[Strict] = Schema.derived
  implicit val label: Schema[Label] = Schema.derived
  implicit val addr: Schema[Addr] = Schema.derived
  implicit val person: Schema[Person] = Schema.derived
  implicit val wrong: Schema[Wrong] = Schema.derived
  implicit val wrap: Schema[Wrap] = Schema.derived
  implicit val wrapStrict: Schema[WrapStrict] = Schema.derived
  implicit val in: Schema[In] = Schema.derived
  implicit val out: Schema[Out] = Schema.derived
  implicit val ledger: Schema[Ledger] = Schema.derived
}
