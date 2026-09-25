package okay.cluster.foreign

// docs/foreign-facade.md's examples, verbatim (TestDocSnippets pins every
// Scala line of the page here); the JVM examples run in the default gate,
// the Python ones are the same lines TestPyFacade runs Live

import okay.given
import okay.codec.Schema
import okay.cluster.{Flow, Flows}

final case class Order(sku: String, qty: Long) derives Schema
final case class Priced(sku: String, total: Double) derives Schema

object DocShop:
  val shop = JvmModule("shop")
    .fn[Order, Priced]("price")(o => Priced(o.sku, o.qty * 2.5))
    .frame("priceAll")(identity)

class TestDocExamplesForeignFacade extends munit.FunSuite:
  import DocShop.shop

  test("one value, one call — tier 1, through the facade, on the JVM") {
    val one = Road.value[JvmModule, Order, Priced](shop, "price")(Order("tea", 2L))
    assertEquals(one, Right(Priced("tea", 5.0)))
  }

  test("rows are one frame — tier 2; a Flow is one frame per chunk — tier 3") {
    val orders = Vector(Order("tea", 2L), Order("milk", 1L))
    val priced = Road.rows[JvmModule, Order, Order](shop, "priceAll")(orders)
    assertEquals(priced, Right(orders))
    val flow = Road.flow[JvmModule, Order, Order](shop, "priceAll", 4096)(Flow.slices(orders, 1))
    assertEquals(Flows.collect(flow).runWith.toVector, orders)
  }

  test("what this module's worker does") {
    val report = summon[Speaks[JvmModule]].speaks(shop)
    assertEquals(report.frames, "by-reference")
  }

  test("a capability a module type lacks does not compile") {
    // `Mute` lives in TestFacade: a module type with no instance at all
    assert(compileErrors("Road.value[Mute, Order, Priced](Mute(\"m\"), \"price\")(Order(\"tea\", 1L))").nonEmpty)
    assert(compileErrors("summon[Methods[okay.r.RModule]]").nonEmpty)
  }
