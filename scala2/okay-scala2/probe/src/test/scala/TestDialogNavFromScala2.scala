package scala2probe

import okay.codec.Schema
import okay.scala2._
import okay.ui.{Event, Nav, Screen, Ui}

object DialogModel {
  // a scenario as one program: two questions, then an answer
  val greet: String ! Dialog = for {
    first <- Dialog.show(Ui.Column(Vector(Ui.Text("hello?"), Ui.Button("yes", "yes"), Ui.Button("no", "no"))))
    answer <- first match {
      case Event.Pressed("yes") => Dialog.show(Ui.Input("", "name", "your name")).map {
        case Event.Edited(_, name) => "hi " + name
        case _ => "hi"
      }
      case _ => pure("bye")
    }
  } yield answer

  final case class Age(years: Int)
  object Age {
    implicit val schema: Schema[Age] = Schemas.product1("Age", "years")(Age.apply)(_.years)
  }

  // screens implemented in Scala 2: okay-ui's Screen is a plain trait
  def list: Screen = new Screen {
    def view: Ui = Ui.Column(Vector(Ui.Text("list"), Ui.Button("open", "open")))
    def step(e: Event): Nav = e match {
      case Event.Pressed("open") => Nav.Push(counter)
      case _ => Nav.Stay(this)
    }
  }

  val counter: Screen = Screens.of(0)(n => Ui.Column(Vector(Ui.Text("count " + n), Ui.Button("+", "inc"), Ui.Button("back", "back")))) {
    case (n, Event.Pressed("inc")) => Right(n + 1)
    case (_, Event.Pressed("back")) => Left(Nav.Pop)
    case (n, _) => Right(n)
  }
}

/** okay-ui's Dialog and Nav from Scala 2.13 (specs/scala2-facade.md, stage 14) */
class TestDialogNavFromScala2 extends munit.FunSuite {
  import DialogModel._

  test("a scenario replayed without a host: each screen it drew, and its answer") {
    val (drawn, answer) = Dialog.replay(greet, Seq(Event.Pressed("yes"), Event.Edited("name", "ada")))
    assertEquals(answer, Some("hi ada"))
    assertEquals(drawn.size, 2)
    assertEquals(Dialog.replay(greet, Seq(Event.Pressed("no")))._2, Some("bye"))
    assertEquals(Dialog.replay(greet, Seq.empty)._2, None)
  }

  test("Dialog.ask draws a form from the Schema, submits with $ok, and $cancel answers None") {
    val (_, answer) = Dialog.replay(Dialog.ask[Age]("how old?"), Seq(Event.Edited("years", "36"), Event.Pressed("$ok")))
    assertEquals(answer, Some(Some(Age(36))))
    assertEquals(Dialog.replay(Dialog.ask[Age]("how old?"), Seq(Event.Pressed("$cancel")))._2, Some(None))
  }

  test("a scenario runs on a host") {
    val host = ScriptedHost(Event.Pressed("yes"), Event.Edited("name", "bo"))
    assertEquals(Dialog.run(host.host)(greet).runWith, Some("hi bo"))
    assertEquals(host.frames.size, 2)
  }

  test("screens as a stack, run by the ordinary UiApp loop") {
    val host = ScriptedHost(Event.Pressed("open"), Event.Pressed("inc"), Event.Pressed("inc"))
    val stack = UiApp.run(Nav.state(list))(Nav.view)(Nav.update)(host.host).runWith
    assertEquals(stack.size, 2)
    assertEquals(Nav.view(stack), counter.step(Event.Pressed("inc")) match {
      case Nav.Stay(s) => s.step(Event.Pressed("inc")) match { case Nav.Stay(s2) => s2.view; case other => fail(other.toString) }
      case other => fail(other.toString)
    })
    val back = Nav.update(stack, Event.Pressed("back"))
    assertEquals(back.size, 1)
  }
}
