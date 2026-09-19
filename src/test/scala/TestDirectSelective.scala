package okay

import okay.Direct.{*, given}
// the auto-coloured `if` below goes through `selfColor`, an implicit
// CONVERSION, so the file needs the language import — see the note in
// TestDirectApplicative.scala
import scala.language.implicitConversions

/**
 * An `if` whose CONDITION is an effect, in a direct block at a
 * carrier with no monad (specs/applicative-do.md, the Selective rung).
 *
 * This is the one shape an applicative cannot express. `<*>` runs
 * both of its arguments, so a branch would happen whether or not it
 * was taken — and for a validator that means reporting a bad shipping
 * address on an order that was never going to be shipped. `ifS` runs
 * the scrutinee and then at most one side.
 */
class TestDirectSelective extends munit.FunSuite {

  private given Semigroup[Seq[String]] with
    def combine(x: Seq[String], y: Seq[String]): Seq[String] = x ++ y

  type Errors = Seq[String]
  final case class Raw(item: String, delivery: String, address: String)
  final case class Order(item: String, ship: String)

  /** what each check DID, so the test can assert that one did not run */
  private val ran = collection.mutable.Buffer.empty[String]

  private def checkItem(s: String): Validated[Errors, String] =
    ran += "item"
    if s.nonEmpty then Validated.Valid(s) else Validated.Invalid(Seq("item is empty"))

  /** the flag is itself a check: it can fail on its own */
  private def wantsDelivery(s: String): Validated[Errors, Boolean] =
    ran += "delivery"
    s match
      case "yes" => Validated.Valid(true)
      case "no" => Validated.Valid(false)
      case other => Validated.Invalid(Seq(s"delivery must be yes or no, not '$other'"))

  private def checkAddress(s: String): Validated[Errors, String] =
    ran += "address"
    if s.nonEmpty then Validated.Valid(s) else Validated.Invalid(Seq("address is empty"))

  private def pickup: Validated[Errors, String] =
    ran += "pickup"
    Validated.Valid("pickup at the store")

  private def order(raw: Raw): Validated[Errors, Order] = direct:
    val item = checkItem(raw.item)
    val ship = if wantsDelivery(raw.delivery) then checkAddress(raw.address) else pickup
    Order(item, ship)

  test("THE POINT: a bad address is not reported on an order that is not shipped") {
    ran.clear()
    val got = order(Raw("book", "no", ""))          // empty address, but pickup
    assertEquals(got, Validated.Valid(Order("book", "pickup at the store")))
    assertEquals(ran.toList, List("item", "delivery", "pickup"))
    assert(!ran.contains("address"), "the address was checked on a pickup order")
  }

  test("and it IS reported when the order is shipped") {
    ran.clear()
    assertEquals(order(Raw("book", "yes", "")),
      Validated.Invalid(Seq("address is empty")))
    assertEquals(ran.toList, List("item", "delivery", "address"))
  }

  test("the independent checks still accumulate around the conditional") {
    ran.clear()
    assertEquals(order(Raw("", "yes", "")),
      Validated.Invalid(Seq("item is empty", "address is empty")))
  }

  test("a failing CONDITION reports itself and runs neither branch") {
    ran.clear()
    assertEquals(order(Raw("book", "maybe", "")),
      Validated.Invalid(Seq("delivery must be yes or no, not 'maybe'")))
    assertEquals(ran.toList, List("item", "delivery"))
  }

  test("the whole order is valid when everything is") {
    ran.clear()
    assertEquals(order(Raw("book", "yes", "1 Main St")),
      Validated.Valid(Order("book", "1 Main St")))
  }
}
