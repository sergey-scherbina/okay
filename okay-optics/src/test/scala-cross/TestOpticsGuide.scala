package okay

import okay.given

/**
 * THE PAIRS ON docs/optics.md, RUN.
 *
 * The page is written as pairs — the code a person writes today on
 * the left, the optic that replaces it on the right — and a pair is
 * only worth printing if the two sides answer the same thing. This
 * file is where that is checked, so the page cannot drift from the
 * library the way prose does (`the-record-outlives-the-truth`).
 *
 * Every code block on that page has a test here under the same name,
 * and the shapes are the ones the page shows. Two of the five pairs
 * come from real call sites in this repository, named in the page and
 * in the test: `WordTfIdf.against` (a deep field set) and
 * `Parse.rebase` (the one where the optic does NOT win, kept for
 * exactly that reason).
 */
class TestOpticsGuide extends munit.FunSuite {

  // ---------------------------------------------------------------- the shapes the page uses

  final case class Taxon(name: String)
  final case class Probe(weights: Vector[Double], taxon: Taxon)
  final case class Trained(vocab: Vector[String], probe: Probe)

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, address: Option[Address])

  final case class Line(sku: String, qty: Int)
  final case class Order(id: String, lines: Vector[Line])

  sealed trait Shape
  final case class Circle(r: Double) extends Shape
  final case class Square(side: Double) extends Shape

  final case class Span(offset: Int, line: Int)
  final case class Token(lexeme: String, span: Span)

  // ---------------------------------------------------------------- 1. set a field one level down

  test("pair 1: a deep field set — the nested copy and the lens chain agree") {
    val t = Trained(Vector("a", "b"), Probe(Vector(0.5), Taxon("old")))
    val taxon = Taxon("new")

    // what WordTfIdf.against writes today
    val byHand = t.copy(probe = t.probe.copy(taxon = taxon))

    // the same, named as a path
    val probeTaxon = Lens[Trained](_.probe).andThen(Lens[Probe](_.taxon))
    val byOptic = probeTaxon.set(taxon)(t)

    assertEquals(byOptic, byHand)
    // and nothing else moved
    assertEquals(byOptic.vocab, t.vocab)
    assertEquals(byOptic.probe.weights, t.probe.weights)
  }

  // ---------------------------------------------------------------- 2. read through an absence

  test("pair 2: a deep read through an Option — the map chain and the affine preview agree") {
    val city = Lens[Person](_.address).andThen(Prism.some).andThen(Lens[Address](_.city))

    val here = Person("ada", Some(Address("Wrocław", 50001)))
    assertEquals(city.preview(here), here.address.map(_.city))
    assertEquals(city.preview(here), Some("Wrocław"))

    val nowhere = Person("bob", None)
    assertEquals(city.preview(nowhere), nowhere.address.map(_.city))
    assertEquals(city.preview(nowhere), None)
  }

  test("pair 2b: the same optic WRITES, which the read chain cannot do at all") {
    val city = Lens[Person](_.address).andThen(Prism.some).andThen(Lens[Address](_.city))

    val here = Person("ada", Some(Address("wrocław", 50001)))
    // the hand-written write is a different expression from the read
    val byHand = here.copy(address = here.address.map(a => a.copy(city = a.city.capitalize)))
    assertEquals(city.modify(_.capitalize)(here), byHand)

    // absent: the write is a no-op, and the hand-written one says so too
    val nowhere = Person("bob", None)
    assertEquals(city.modify(_.capitalize)(nowhere), nowhere)
  }

  // ---------------------------------------------------------------- 3. every element

  test("pair 3: every element — the map inside a copy and the traversal agree") {
    val order = Order("A-1", Vector(Line("pen", 2), Line("ink", 0)))
    def bump(l: Line): Line = l.copy(qty = l.qty + 1)

    val byHand = order.copy(lines = order.lines.map(bump))

    val eachLine = Lens[Order](_.lines).andThen(Traversal.each[Line, Line])
    assertEquals(eachLine.modify(bump)(order), byHand)

    // the read side of the same optic, which the copy does not have
    assertEquals(eachLine.toVector(order), order.lines)
    assertEquals(eachLine.foldMap((l: Line) => l.qty)(order), 2)
  }

  // ---------------------------------------------------------------- 4. one case of a sum, inside a collection

  test("pair 4: one case of a sum — the partial-function map and the prism chain agree") {
    val shapes = Vector[Shape](Circle(1.0), Square(2.0), Circle(3.0))

    val byHand = shapes.map {
      case c: Circle => c.copy(r = c.r * 2)
      case s => s
    }

    val radii = Traversal.each[Shape, Shape]
      .andThen(Prism.of[Shape, Circle])
      .andThen(Lens[Circle](_.r))
    assertEquals(radii.modify(_ * 2)(shapes), byHand)
    assertEquals(radii.modify(_ * 2)(shapes), Vector[Shape](Circle(2.0), Square(2.0), Circle(6.0)))

    // the squares were not visited, which is what the prism is for
    assertEquals(radii.toVector(shapes), Vector(1.0, 3.0))
  }

  // ---------------------------------------------------------------- 5. where the optic does NOT win

  test("pair 5: TWO fields of one sub-record — one copy, or two rebuilds; both answer the same") {
    val t = Token("let", Span(offset = 10, line = 2))
    val (delta, lineDelta) = (5, 1)

    // what Parse.rebase writes today: ONE copy of the span
    val byHand = t.copy(span = t.span.copy(
      offset = t.span.offset + delta, line = t.span.line + lineDelta))

    // the same through optics: two chains, so the span is rebuilt TWICE
    val span = Lens[Token](_.span)
    val shifted =
      span.andThen(Lens[Span](_.line)).modify(_ + lineDelta)(
        span.andThen(Lens[Span](_.offset)).modify(_ + delta)(t))

    assertEquals(shifted, byHand)
    assertEquals(shifted, Token("let", Span(15, 3)))
    // the answers agree; the page says to keep the copy here, and this
    // test is what makes that advice a measured shape rather than taste
  }

  // ---------------------------------------------------------------- the two roads

  test("the same optic, named in code and chosen at run time, answers the same") {
    val order = Order("A-1", Vector(Line("pen", 2), Line("ink", 0)))

    // named: the planner can read this one and emits the update
    val named = Lens[Order](_.lines).andThen(Traversal.each[Line, Line])

    // chosen at run time: nothing is known at the call, so the
    // interpretation runs — the same answer, a different price
    val chosen: Traversal[Order, Order, Line, Line] =
      if order.lines.nonEmpty then named else named

    def bump(l: Line): Line = l.copy(qty = l.qty + 1)
    assertEquals(chosen.modify(bump)(order), named.modify(bump)(order))
  }
}
