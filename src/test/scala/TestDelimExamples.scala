package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * WORKED EXAMPLES OF DELIMITED CONTROL, from the literature, as tests
 * so they cannot rot (delim-examples, 2026-09-16). Each one is a
 * shape the papers use to argue that first-class continuations earn
 * their keep, written here in `direct` style against `Delim`.
 */
class TestDelimExamples extends munit.FunSuite {

  type R = Delim + okay.Pure

  // ---- 1 · reverse-mode automatic differentiation
  //      Wang & Rompf, "Demystifying Differentiable Programming" (2018).
  //      The forward pass is what you write; the BACKWARD pass is what
  //      the continuation does on the way out. No tape, no graph.

  final class Num(val x: Double, var d: Double = 0.0)

  def times(a: Num, b: Num)(using Delim.Prompted[Unit]): Num ! R = direct:
    !Delim.shift[Num]: k =>
      val y = Num(a.x * b.x)
      direct:
        !k(y)                  // the rest of the computation runs...
        a.d += b.x * y.d       // ...and on the way back, the adjoints
        b.d += a.x * y.d

  def plus(a: Num, b: Num)(using Delim.Prompted[Unit]): Num ! R = direct:
    !Delim.shift[Num]: k =>
      val y = Num(a.x + b.x)
      direct:
        !k(y)
        a.d += y.d
        b.d += y.d

  def grad(f: Delim.Prompted[Unit] ?=> Num => Num ! R)(x: Double): Double =
    val v = Num(x)
    !.run(Delim.delimited[Unit, okay.Pure]:
      direct:
        val y = !f(v)
        y.d = 1.0)             // seed the output adjoint
    v.d

  test("reverse-mode AD: the backward pass IS the continuation") {
    // f(x) = x*x + 3x, so f'(x) = 2x + 3
    def f(using Delim.Prompted[Unit])(v: Num): Num ! R = direct:
      val sq = !times(v, v)
      val lin = !times(Num(3.0), v)
      !plus(sq, lin)
    for x <- List(0.0, 2.0, 5.0) do
      assertEqualsDouble(grad(f(using summon))(x), 2 * x + 3, 1e-9, s"at $x")
  }

  // ---- 2 · a generator: a recursive walk, read as a sequence
  //      The shape every effect-handlers paper opens with — the walk
  //      is ordinary recursion, and nothing is inverted.

  enum Tree[+A]:
    case Leaf(a: A)
    case Node(l: Tree[A], r: Tree[A])

  type Items = List[Int]

  def yieldOne(a: Int)(using Delim.Prompted[Items]): Unit ! R = direct:
    !Delim.shift[Unit](k => direct { a :: !k(()) })

  def walk(t: Tree[Int])(using Delim.Prompted[Items]): Unit ! R = direct:
    t match
      case Tree.Leaf(a) => !yieldOne(a)
      case Tree.Node(l, r) =>
        !walk(l)
        !walk(r)

  def elements(t: Tree[Int]): List[Int] =
    !.run(Delim.delimited[Items, okay.Pure]:
      direct:
        !walk(t)
        List.empty[Int])

  test("generator: the walk yields, and the caller reads a sequence") {
    val t = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Leaf(3))
    assertEquals(elements(t), List(1, 2, 3))
    assertEquals(elements(Tree.Leaf(7)), List(7))
  }

  // ---- 3 · a web dialogue
  //      Queinnec, "The influence of browsers on evaluators" (2000):
  //      the program asks, and the REST of the dialogue is a value
  //      kept until the answer arrives. No state machine, no session.

  enum Page:
    case Ask(question: String, resume: String => Page)
    case Done(text: String)

  def ask(q: String)(using Delim.Prompted[Page]): String ! R = direct:
    !Delim.shift[String]: k =>
      okay.pure(Page.Ask(q, (answer: String) => !.run(Delim.run(k(answer)))))

  def booking(using Delim.Prompted[Page]): Page ! R = direct:
    val city = !ask("Which city?")
    val days = !ask(s"How many nights in $city?")
    val pay = !ask(s"Pay ${days.toInt * 90} for $city?")
    if pay == "yes" then Page.Done(s"Booked $city for $days nights")
    else Page.Done("Cancelled")

  test("web dialogue: the continuation outlives the request") {
    def answer(p: Page, as: List[String]): Page = (p, as) match
      case (Page.Ask(_, resume), a :: rest) => answer(resume(a), rest)
      case (done, _) => done
    val start = !.run(Delim.delimited[Page, okay.Pure](booking))
    assertEquals(answer(start, List("Kyiv", "3", "yes")),
      Page.Done("Booked Kyiv for 3 nights"))
    // the SAME start page, answered differently — the dialogue is a value
    assertEquals(answer(start, List("Lviv", "2", "no")), Page.Done("Cancelled"))
  }

  // ---- 4 · answer-type modification
  //      Danvy & Filinski (1990): the block produces an Int and the
  //      delimiter answers a String. `Cont[A, S, R]` carries that; a
  //      plain monad cannot say it.

  test("answer-type modification: the answer changes type along the way") {
    val r: String = reset[Int, String](
      shift[Int, Int, String](k => s"answer: ${k(20) + 2}").map(_ + 20))
    assertEquals(r, "answer: 42")
  }
}
