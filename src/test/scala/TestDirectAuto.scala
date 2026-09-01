package okay

import okay.Direct.{*, given}
import scala.language.implicitConversions

/** Auto-coloring: specs/direct-auto-coloring.md */
class TestDirectAuto extends munit.FunSuite {

  given Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)

  test("ascribed positions color: val x: Int = m") {
    def add(mx: Option[Int], my: Option[Int]): Option[Int] =
      direct[Option] {
        val x: Int = mx
        val y: Int = my
        x + y
      }
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(Some(2), None), None)
    assertEquals(add(None, Some(3)), None)
  }

  test("argument and selection positions color") {
    def twice(i: Int): Int = i * 2
    val r = direct[Option] { twice(Option(3)) + (Option(4): Int) }
    assertEquals(r, Some(10))
  }

  test("operations color via the Effect marker; Unit ops keep the explicit mark") {
    type F = Reader % Int + Writer % String
    given Effect[[X] =>> Reader[Int, X]] with {}
    // auto-coloring resolves at the DECLARED type: a smart
    // constructor typed at the trait colors; a raw case constructor
    // (Reader.Ask[Int, Int]()) is too precise for G inference
    def ask: Reader[Int, Int] = Reader.Ask()
    val prog: Int ! F = direct {
      val env: Int = ask                      // colored via the marker
      Writer(s"env=$env")                     // bare statement: do-notation,
                                              // the statement IS the mark
      env + 1
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](41)(prog)))
    assertEquals(ws, Seq("env=41"))
    assertEquals(a, 42)
  }

  test("do-notation: a bare statement of the block's monad runs") {
    assertEquals(direct[Option] { Option(1); 2 }, Some(2))
    assertEquals(direct[Option] { (None: Option[Int]); 2 }, None)
    assertEquals(direct[Option] { None; 2 }, None)
  }

  test("do-notation: a bare List statement re-runs the rest per element") {
    given Monad[List] with
      override def pure[A](a: A): List[A] = List(a)
      extension [A](m: List[A])
        override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)
    var runs = 0
    val r = direct[List] {
      List(1, 2, 3)
      runs += 1
      7
    }
    assertEquals(r, List(7, 7, 7))
    assertEquals(runs, 3)
  }

  test("a foreign marked type in statement position still refuses") {
    val e = compileErrors(
      "import okay.Direct.given; import scala.language.implicitConversions; " +
        "given okay.Direct.Effect[List] with {}; " +
        "okay.Direct.direct[Option] { List(1); 2 }(using summon[Monad[Option]]) ")
    assert(e.contains("neither"), e)
  }

  test("val keeps the program un-run — binding is consent to hold the value") {
    var built = 0
    def make: Option[Int] = { built += 1; Some(1) }
    val r = direct[Option] {
      val held = make      // the Option VALUE, not its content: no ascription
      held.?               // runs where marked
    }
    assertEquals(r, Some(1))
    assertEquals(built, 1)
  }

  test("outside a direct block nothing colors") {
    val e = compileErrors("val x: Int = Option(1) ")
    assert(e.nonEmpty)
  }

  test("an unmarked G never colors") {
    val e = compileErrors(
      "import okay.Direct.{*, given}; import scala.language.implicitConversions; " +
        "okay.Direct.direct[Option] { val x: Int = List(1); x }(using summon[Monad[Option]]) ")
    assert(e.nonEmpty)
  }

  test("explicit marks mix freely with auto-coloring") {
    val r = direct[Option] {
      val x: Int = Option(1)
      val y = Option(2).?
      x + y
    }
    assertEquals(r, Some(3))
  }

  test("multi-shot survives auto-coloring") {
    given Monad[List] with
      override def pure[A](a: A): List[A] = List(a)
      extension [A](m: List[A])
        override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)
    val r = direct[List] {
      val x: Int = List(1, 2)
      val y: Int = List(10, 20)
      x * y
    }
    assertEquals(r, List(10, 20, 20, 40))
  }
}
