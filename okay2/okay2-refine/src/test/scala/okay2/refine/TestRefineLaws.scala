package okay2.refine

import okay2.codec.Json
import RefineLaws.{Expect, Finding}

/** okay's refine-laws on the Scala 2 core: a lawful pattern passes, each way of breaking one is found */
class TestRefineLaws extends munit.FunSuite {

  val int: Refine[String, Int] =
    Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)

  test("a lawful pattern: every law holds, on inputs and on values") {
    val r = RefineLaws.check(int, Seq("1", "42", "-7", "x"), values = Seq(0, 99))
    assert(r.ok, r)
    assertEquals((r.inputs, r.took, r.values), (4, 3, 2))
  }

  test("a record through `and` over Json reads back") {
    import Refine.json._
    val money = (field("amount") >>> num) and (field("currency") >>> str)
    assert(RefineLaws.check(money, Seq(Json.parse("""{"amount": 5, "currency": "EUR"}""")), values = Seq((1.5, "USD"))).ok)
  }

  test("a write that loses what it read, a refused write, a throw, a drift: each is found") {
    val lossy = Refine.step[String, Int]("lossy")(s => s.toIntOption.toRight("no"))(_ => "0")
    assertEquals(RefineLaws.check(lossy, Seq("0", "5")).findings, Vector(Finding.ReadBackDiffers("input #2", "5", "0")))
    val num: Refine[String, AnyVal] = int.widen[AnyVal]
    assert(RefineLaws.check(num, Nil, values = Seq[AnyVal](42, true)).findings.exists(_.isInstanceOf[Finding.WriteRefused]))
    val boom = Refine.step[String, Int]("boom")(s => if (s == "!") throw new IllegalStateException("kaboom") else Right(s.length))(n => "x" * n)
    assert(RefineLaws.check(boom, Seq("!")).findings.contains(Finding.Threw("input #1", "read", "IllegalStateException: kaboom")))
    var n = 0
    val drifting = Refine.step[String, Int]("drift")(_ => { n += 1; Right(n) })(_.toString)
    assert(RefineLaws.check(drifting, Seq("a")).findings.exists(_.isInstanceOf[Finding.NotDeterministic]))
  }

  test("corpus mode: every input NOT taken is a finding, with its refusal or its readings") {
    val even = Refine.step[Int, Int]("even")(n => if (n % 2 == 0) Right(n) else Left(s"$n is odd"))(identity)
    val small = Refine.step[Int, Int]("small")(n => if (n < 10) Right(n) else Left(s"$n is not small"))(identity)
    val r = RefineLaws.checkNamed(int >>> (even or small), Seq("four" -> "4", "eleven" -> "11", "twelve" -> "12"), expect = Expect.Corpus)
    assertEquals(r.findings, Vector(
      Finding.NotTaken("four", "unclear: int/even | int/small"),
      Finding.NotTaken("eleven", "declined: int/even: 11 is odd; int/small: 11 is not small")))
  }
}
