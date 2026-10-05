package okay.semantic

class TestSemanticExample extends okay.testkit.Munit.Diagnosed:
  test("an application defines revenue once and executes without agents") {
    case class Order(month: String, eur: BigDecimal)
    val result = for
      model <- Model.build[Order]("orders", Origin("accounting", "v1"), "one order",
        Vector(Dimension("month", "Recognition month", Kind.Text, o => Value.Text(o.month))),
        Vector(Measure("amount", "Recognized amount in EUR", o => Some(o.eur))),
        Vector(Metric("revenue", "Recognized revenue", "EUR", Calculation.Sum("amount"))))
      plan <- model.plan(Request(Vector("revenue"), Vector("month")))
      result <- plan.run(Vector(Order("2026-01", BigDecimal("10.50")), Order("2026-01", BigDecimal("2.25"))))
    yield result
    note(s"revenue example: $result")
    assertEquals(result.toOption.get.groups,
      Vector(Group(Vector(Value.Text("2026-01")), Vector(Some(BigDecimal("12.75"))))))
  }
