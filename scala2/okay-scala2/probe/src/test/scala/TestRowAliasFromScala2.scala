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

  test("a program at a + row of a user effect and two built-ins, handled effect by effect") {
    val prog: Eff[Effect[Console] + State[Int] + Writer[String], String] = for {
      name <- Console.send(ReadLn)
      _ <- State.put(name.length)
      _ <- Writer.tell("got " + name)
      _ <- Console.send(PrintLn("hi " + name))
    } yield name
    val out = ListBuffer.empty[String]
    val console = new Handler[Console, State[Int] + Writer[String], String] {
      def apply[X](op: Console[X], k: X => Eff[State[Int] + Writer[String], String]) = op match {
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
    val one: Eff[State[Int], Int] = State.get[Int]
    val wider: Eff[Reader[String] + State[Int] + Throws[String], Int] = one
    assertEquals(Eff.run(Throws.run(State.run(4)(Reader.run("env")(wider)))), Right((4, 4)))
  }
}
