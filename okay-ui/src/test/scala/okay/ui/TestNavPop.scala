package okay.ui



/**
 * The named boundary (specs/ui.md, nav-pop-to-screen): pop across
 * untouched intermediates, nested boundaries, plain Nav unchanged —
 * and the mechanism honesty: the stack is data, the exit is a drop.
 */
class TestNavPop extends munit.FunSuite {

  /** a probe screen: records every step it is asked to make */
  def probe(name: String, stepped: collection.mutable.Buffer[String],
            onPress: PartialFunction[String, Nav]): Screen = new Screen:
    def view: Ui = Ui.Text(name)
    def step(e: Event): Nav =
      stepped += name
      e match
        case Event.Pressed(k) if onPress.isDefinedAt(k) => onPress(k)
        case _ => Nav.Stay(this)

  test("pop-to-named crosses two intermediate screens, each untouched by the exit") {
    val stepped = collection.mutable.Buffer[String]()
    val done = collection.mutable.Buffer[String]()
    val k = Nav.key[String]
    val home = Nav.boundary(k, probe("home", stepped, PartialFunction.empty)) { a =>
      done += a
      Nav.To(probe(s"result:$a", stepped, PartialFunction.empty))
    }
    val mid = probe("mid", stepped, PartialFunction.empty)
    val top = probe("top", stepped, { case "bail" => Nav.PopTo(k, "answer-42") })

    var stack: List[Screen] = List(top, mid, home)
    stack = Nav.update(stack, Event.Pressed("bail"))
    assertEquals(done.toList, List("answer-42"))
    assertEquals(Nav.view(stack), Ui.Text("result:answer-42"))
    assertEquals(stack.length, 1)
    // ONLY top stepped; mid and home were dropped/routed as DATA
    assertEquals(stepped.toList, List("top"))
  }

  test("two named boundaries nest; the inner pop stops at the inner") {
    val stepped = collection.mutable.Buffer[String]()
    val outerK = Nav.key[String]; val innerK = Nav.key[Int]
    val outer = Nav.boundary(outerK, probe("outer", stepped, PartialFunction.empty))(
      a => Nav.To(probe(s"outer-got:$a", stepped, PartialFunction.empty)))
    val inner = Nav.boundary(innerK, probe("inner", stepped, PartialFunction.empty))(
      n => Nav.To(probe(s"inner-got:$n", stepped, PartialFunction.empty)))
    val top = probe("top", stepped, {
      case "in" => Nav.PopTo(innerK, 7)
      case "out" => Nav.PopTo(outerK, "far")
    })

    val s1 = Nav.update(List(top, inner, outer), Event.Pressed("in"))
    assertEquals(Nav.view(s1), Ui.Text("inner-got:7"))
    assertEquals(s1.length, 2)                     // outer boundary survives below

    // and the outer pop CROSSES the inner boundary — the capability
    val s2 = Nav.update(List(top, inner, outer), Event.Pressed("out"))
    assertEquals(Nav.view(s2), Ui.Text("outer-got:far"))
    assertEquals(s2.length, 1)
  }

  test("a plain Nav program never meets any of this; an absent key names nothing") {
    val stepped = collection.mutable.Buffer[String]()
    val plain = probe("plain", stepped, { case "go" => Nav.Push(probe("next", stepped, PartialFunction.empty)) })
    val s1 = Nav.update(List(plain), Event.Pressed("go"))
    assertEquals(Nav.view(s1), Ui.Text("next"))
    // PopTo to a key that is NOT on the stack: total, unchanged
    val ghost = Nav.key[String]
    val bail = probe("bail", stepped, { case "x" => Nav.PopTo(ghost, "?") })
    val s2 = Nav.update(List(bail, plain), Event.Pressed("x"))
    assertEquals(s2.map(_.view), List(Ui.Text("bail"), Ui.Text("plain")))
  }
}
