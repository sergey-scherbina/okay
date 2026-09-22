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

  // ---- 5 · functional unparsing, in plain CPS
  //      Danvy, "Functional Unparsing" (JFP 1998): a format is a
  //      VALUE, built from directives, and the TYPE of sprintf is
  //      COMPUTED FROM IT. No macro, no string parsing, no varargs —
  //      and no continuations library either: this one is here as the
  //      baseline example 6 has to earn its keep against.

  object Unparse:
    /** a directive takes "what to do with the string so far" */
    type K[A] = String => A

    def done: K[String] = pre => pre
    def lit[A](s: String)(next: K[A]): K[A] = pre => next(pre + s)
    def str[A](next: K[A]): K[String => A] = pre => (x: String) => next(pre + x)
    def int[A](next: K[A]): K[Int => A] = pre => (n: Int) => next(pre + n)

    def sprintf[A](d: K[A]): A = d("")

  test("unparsing: the result type is computed from the format") {
    import Unparse.*
    // "%s is %d years old" — and its type is String => Int => String,
    // which nobody wrote down: the format decided it
    val greeting: String => Int => String =
      sprintf(str(lit(" is ")(int(lit(" years old")(done)))))
    assertEquals(greeting("Ada")(36), "Ada is 36 years old")

    val plain: String = sprintf(lit("no arguments")(done))
    assertEquals(plain, "no arguments")

    // the arity is in the type, so too few arguments does not compile
    assert(compileErrors("""val g: String = Unparse.sprintf(Unparse.str(Unparse.done))""")
      .nonEmpty, "a format expecting an argument typed as a finished String")
    // and so is the argument's TYPE
    assert(compileErrors("""Unparse.sprintf(Unparse.int(Unparse.done))("not a number")""")
      .nonEmpty, "a %d directive accepted a String")
  }

  // ---- 6 · the same thing through shift/reset
  //      Asai, "On typing delimited continuations: three new
  //      solutions to the printf problem" (2007). Each directive is a
  //      `shift` that MOVES THE ANSWER TYPE: `str` turns "the
  //      delimiter answers T" into "it answers String => T". The
  //      format is then just their composition, and the plumbing
  //      example 5 threads by hand is what the continuation is.

  object Fmt:
    def lit[T](s: String): Cont[String, T, T] = shift(k => k(s))
    def str[T]: Cont[String, T, String => T] = shift(k => (x: String) => k(x))
    def int[T]: Cont[String, T, Int => T] = shift(k => (n: Int) => k(n.toString))

  test("printf via shift/reset: the format is a for-comprehension") {
    import Fmt.*
    type Out = String => Int => String

    // NOTHING is annotated inside: the expected type on `reset` carries
    // the whole chain of answer types through the generators
    val greeting: Out = reset[String, Out]:
      for
        x <- lit("Hello, ")
        y <- str
        z <- lit(" is ")
        w <- int
      yield x + y + z + w + " years old"

    assertEquals(greeting("Ada")(36), "Hello, Ada is 36 years old")

    // the order of the directives is the order of the arguments, and
    // the types say so: swap them and it does not compile
    assert(compileErrors("""
      val g: String => Int => String = okay.reset[String, String => Int => String](
        for { x <- Fmt.int; y <- Fmt.str } yield x + y)
    """).nonEmpty, "the directives were accepted in the wrong order")
  }

  test("printf is exactly what a direct block cannot express") {
    // `Cont.direct.shift` takes the answer type from the block, which
    // presumes the block HAS one: a direct block is diagonal, one
    // `F[A]` for all of it. Every directive above moves the answer
    // type, so there is no `AnswerOf` for it — and that is the
    // boundary, stated by the compiler rather than by this comment.
    val moving = compileErrors(
      "summon[okay.Cont.direct.AnswerOf[[X] =>> okay.Cont[X, String, String => String]]]")
    assert(moving.nonEmpty, "a moving answer type was accepted as a direct block's monad")

    // the diagonal, the shape a direct block does have, resolves —
    // and TestContDirect runs a block written that way
    val fixed = compileErrors(
      "summon[okay.Cont.direct.AnswerOf[[X] =>> okay.Cont[X, String, String]]]")
    assertEquals(fixed, "", "the diagonal lost its witness")
  }
}
