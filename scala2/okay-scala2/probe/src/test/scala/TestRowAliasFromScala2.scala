package scala2probe

import okay.scala2._

import scala.collection.mutable.ListBuffer

/** the row alias `+` (specs/scala2-facade.md, stage 16): it IS the
 * intersection, and a row written with it is handled one capability at
 * a time with nothing annotated */
class TestRowAliasFromScala2 extends munit.FunSuite {

  def same[A, B](implicit ev: A =:= B): Unit = { val _ = ev }

  test("R + S is R with S, both ways, for one + and for a chain") {
    same[State[Int] + Writer[String], State[Int] with Writer[String]]
    same[State[Int] with Writer[String], State[Int] + Writer[String]]
    same[Reader[Int] + State[Int] + Throws[String], Reader[Int] with State[Int] with Throws[String]]
    same[Reader[Int] with State[Int] with Throws[String], Reader[Int] + State[Int] + Throws[String]]
    same[Effect[Console] + State[Int], Effect[Console] with State[Int]]
  }

  test("A ! R is Eff[R, A], both ways; every infix type operator has one precedence, so A ! (R + S) needs its parentheses") {
    same[Int ! State[Int], Eff[State[Int], Int]]
    same[Eff[State[Int], Int], Int ! State[Int]]
    same[Int ! (State[Int] + Writer[String]), Eff[State[Int] with Writer[String], Int]]
    same[String ! (Effect[Console] + State[Int] + Writer[String]), Eff[Effect[Console] with State[Int] with Writer[String], String]]
    // SLS 2.13 §3.2.8, unlike Scala 3: without them `!` and `+` associate to the left
    same[Int ! State[Int] + Writer[String], (Int ! State[Int]) + Writer[String]]
    // and it keeps Eff's contravariance in R
    val one: Int ! State[Int] = State.get[Int]
    val wider: Int ! (Reader[String] + State[Int]) = one
    assertEquals(Eff.run(State.run(4)(Reader.run("env")(wider))), (4, 4))
  }

  test("a program at a + row of a user effect and two built-ins, handled effect by effect") {
    val prog: String ! (Effect[Console] + State[Int] + Writer[String]) = for {
      name <- Console.send(ReadLn)
      _ <- State.put(name.length)
      _ <- Writer.tell("got " + name)
      _ <- Console.send(PrintLn("hi " + name))
    } yield name
    val out = ListBuffer.empty[String]
    val console = new Handler[Console, State[Int] + Writer[String], String] {
      def apply[X](op: Console[X], k: X => String ! (State[Int] + Writer[String])) = op match {
        case PrintLn(s) => out += s; k(())
        case ReadLn => k("ada")
      }
    }
    val handled = Console.handle(prog)(a => Eff.pure(a))(console)
    assertEquals(Eff.run(Writer.run(State.run(0)(handled))), (Vector("got ada"), (3, "ada")))
    assertEquals(Eff.run(State.run(0)(Writer.run(handled))), (3, (Vector("got ada"), "ada")))
    assertEquals(out.toList, List("hi ada", "hi ada")) // the program ran twice
  }

  test("a program needing one capability is a program in any + row that has it") {
    val one: Int ! State[Int] = State.get[Int]
    val wider: Int ! (Reader[String] + State[Int] + Throws[String]) = one
    assertEquals(Eff.run(Throws.run(State.run(4)(Reader.run("env")(wider)))), Right((4, 4)))
  }
}
