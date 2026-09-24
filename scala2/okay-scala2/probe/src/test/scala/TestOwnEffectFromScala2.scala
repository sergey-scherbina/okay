package scala2probe

import okay.scala2._

import scala.collection.mutable.ListBuffer

sealed trait Console[A] extends Op[A]
final case class PrintLn(s: String) extends Console[Unit]
case object ReadLn extends Console[String]
object Console extends Effect[Console]

sealed trait Choose[A] extends Op[A]
case object Flip extends Choose[Boolean]
object Choose extends Effect[Choose]

/** a Scala 2 user's own effects (specs/scala2-facade.md, stage 3) */
class TestOwnEffectFromScala2 extends munit.FunSuite {

  def console[R, B](out: ListBuffer[String], input: String): Handler[Console, R, B] =
    new Handler[Console, R, B] {
      def apply[X](op: Console[X], k: X => Eff[R, B]): Eff[R, B] = op match {
        case PrintLn(s) => out += s; k(())
        case ReadLn => k(input)
      }
    }

  test("a resumptive effect beside State, in one program") {
    val prog: Eff[Effect[Console] + State[Int], String] = for {
      name <- Console.send(ReadLn)
      _ <- State.put(name.length)
      _ <- Console.send(PrintLn("hi " + name))
    } yield name
    val out = ListBuffer.empty[String]
    val handled = Console.handle(prog)(a => Eff.pure(a))(console(out, "ada"))
    assertEquals(Eff.run(State.run(0)(handled)), (3, "ada"))
    assertEquals(out.toList, List("hi ada"))
  }

  test("a multi-shot handler: every answer of two flips") {
    val flips: Eff[Effect[Choose], (Boolean, Boolean)] = for {
      a <- Choose.send(Flip)
      b <- Choose.send(Flip)
    } yield (a, b)
    val all = new Handler[Choose, Any, List[(Boolean, Boolean)]] {
      def apply[X](op: Choose[X], k: X => Eff[Any, List[(Boolean, Boolean)]]): Eff[Any, List[(Boolean, Boolean)]] =
        op match {
          case Flip => for { t <- k(true); f <- k(false) } yield t ++ f
        }
    }
    assertEquals(Choose.run(flips)(p => Eff.pure(List(p)))(all),
      List((true, true), (true, false), (false, true), (false, false)))
  }

  test("an aborting handler drops the continuation: the rest never runs") {
    var rest = false
    val prog: Eff[Effect[Console], Int] =
      Console.send(ReadLn).flatMap(_ => Console.send(PrintLn("x"))).map { _ => rest = true; 1 }
    val abort = new Handler[Console, Any, Option[Int]] {
      def apply[X](op: Console[X], k: X => Eff[Any, Option[Int]]): Eff[Any, Option[Int]] = Eff.pure(None)
    }
    assertEquals(Console.run(prog)(a => Eff.pure(Option(a)))(abort), None)
    assert(!rest)
  }

  test("two user effects in one row, each handled by its own object") {
    val prog: Eff[Effect[Console] + Effect[Choose], String] = for {
      b <- Choose.send(Flip)
      _ <- Console.send(PrintLn("flipped " + b))
    } yield if (b) "heads" else "tails"
    val out = ListBuffer.empty[String]
    val first = new Handler[Choose, Effect[Console], String] {
      def apply[X](op: Choose[X], k: X => Eff[Effect[Console], String]): Eff[Effect[Console], String] =
        op match { case Flip => k(true) }
    }
    val onlyConsole = Choose.handle(prog)(a => Eff.pure(a))(first)
    assertEquals(Console.run(onlyConsole)(a => Eff.pure(a))(console(out, "")), "heads")
    assertEquals(out.toList, List("flipped true"))
  }

  test("an effect left unhandled does not compile") {
    val errors = compileErrors("Eff.run(Console.send(ReadLn))")
    assert(errors.contains("type mismatch"), errors)
    val notLast = compileErrors("Choose.run(Console.send(ReadLn).flatMap(_ => Choose.send(Flip)))(b => Eff.pure(b))(null)")
    assert(notLast.contains("type mismatch"), notLast)
  }
}
