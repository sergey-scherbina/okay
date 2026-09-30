package okay.refine

import java.nio.charset.StandardCharsets.UTF_8
import okay.codec.Json
import okay.testkit.Munit.Diagnosed
import RefineLaws.{Expect, Finding}

/** specs/refine.md, refine-laws: the checker passes a lawful pattern and FINDS each way of breaking one */
class TestRefineLaws extends Diagnosed:

  val int: Refine[String, Int] =
    Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)

  test("a lawful pattern: every law holds, on inputs and on values") {
    val r = RefineLaws.check(int, Seq("1", "42", "-7", "x"), values = Seq(0, 99))
    note(r.toString)
    assert(r.ok, r)
    assertEquals((r.inputs, r.took, r.values), (4, 3, 2))
  }

  test("ISDA's two FpML examples through every level: read, written back as JSON, read again to the same value") {
    val fromBytes = Format.detect >>> Format.value >>> fpml.Fpml.instrument
    val r = RefineLaws.checkNamed(fromBytes,
      Seq("ird-ex01" -> fpml.Samples.vanillaSwap.getBytes(UTF_8), "fx-ex03" -> fpml.Samples.fxForward.getBytes(UTF_8)),
      expect = Expect.Corpus)
    note(r.toString)
    assert(r.ok, r)
    assertEquals(r.took, 2)
  }

  test("a record through `and`: the product's write merges and reads back") {
    import Refine.json.*
    val money = (field("amount") >>> num) and (field("currency") >>> str)
    assert(RefineLaws.check(money, Seq(Json.parse("""{"amount": 5, "currency": "EUR"}""")), values = Seq((1.5, "USD"))).ok)
  }

  test("a write that LOSES what it read is found: the value read back differs") {
    val lossy = Refine.step[String, Int]("lossy")(s => s.toIntOption.toRight("no"))(_ => "0")
    RefineLaws.check(lossy, Seq("0", "5")).findings match
      case Vector(Finding.ReadBackDiffers("input #2", "5", "0")) => ()
      case other => fail(s"expected one read-back difference, got $other")
  }

  test("a value no branch writes is found: the write refused, in its own words") {
    val num: Refine[String, AnyVal] = int.widen[AnyVal]
    RefineLaws.check(num, Nil, values = Seq(42, true)).findings match
      case Vector(Finding.WriteRefused("value #2", "true", why)) => assert(why.contains("int"), why)
      case other => fail(s"expected one refused write, got $other")
  }

  test("a read that THROWS is found, and so is a write that throws") {
    val boom = Refine.step[String, Int]("boom")(s => if s == "!" then throw IllegalStateException("kaboom") else Right(s.length))(n => if n == 3 then sys.error("no threes") else "x" * n)
    val found = RefineLaws.check(boom, Seq("ab", "!", "abc")).findings
    assert(found.contains(Finding.Threw("input #2", "read", "IllegalStateException: kaboom")), found)
    assert(found.exists { case Finding.Threw("input #3", "write", e) => e.contains("no threes"); case _ => false }, found)
  }

  test("a read that is not deterministic is found") {
    var n = 0
    val drifting = Refine.step[String, Int]("drift")(_ => { n += 1; Right(n) })(_.toString)
    assert(RefineLaws.check(drifting, Seq("a")).findings.exists(_.isInstanceOf[Finding.NotDeterministic]))
  }

  test("corpus mode: every input NOT taken is a finding — declined with its refusal, unclear with its readings") {
    val even = Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left(s"$n is odd"))(identity)
    val small = Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left(s"$n is not small"))(identity)
    val r = RefineLaws.checkNamed(int >>> (even or small), Seq("four" -> "4", "eleven" -> "11", "twelve" -> "12"), expect = Expect.Corpus)
    note(r.toString)
    assertEquals(r.findings, Vector(
      Finding.NotTaken("four", "unclear: int/even | int/small"),
      Finding.NotTaken("eleven", "declined: int/even: 11 is odd; int/small: 11 is not small")))
    assert(RefineLaws.checkNamed(int >>> (even or small), Seq("eleven" -> "11")).ok, "Laws mode: a declined input breaks no law")
  }

  test("the report reads as a summary a failing test can print") {
    val lossy = Refine.step[String, Int]("lossy")(s => s.toIntOption.toRight("no"))(_ => "0")
    val text = RefineLaws.check(lossy, Seq("5")).toString
    assert(text.startsWith("RefineLaws: 1 input(s), 1 taken, 0 value(s) — 1 finding(s)"), text)
    assert(text.contains("ReadBackDiffers(input #1,5,0)"), text)
  }
