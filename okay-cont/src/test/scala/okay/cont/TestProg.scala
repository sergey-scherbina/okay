package okay.cont

import okay.{Effects, Free, Pure, Eager, Produce, !, +}
import okay.given
import Prog.given

/** specs/freer-min.md, stage 30: the machine as an `Effects` encoding, chosen by the given */
class TestProg extends okay.testkit.Munit.Diagnosed:

  def prog[M[_[+_], _]](using E: Effects[M]): M[Produce, Int] =
    E.perform[Produce, Int](1).flatMap(x => E.perform[Produce, Int](x + 1).map(y => x + y))

  test("the tagless encodings agree: Prog, Free, Eager — summoned with no import; the syntax by `Prog.given`"):
    import Eager.given
    assertEquals(summon[Effects[Prog]].runWith(prog[Prog]), 3)
    assertEquals(prog[Prog].runWith, 3)
    assertEquals(prog[Free].runWith, 3)
    assertEquals(prog[Eager].runWith, 3)

  test("tailcall: a million deferred calls between two functions, in constant stack"):
    def isEven[M[_[+_], _] : Effects](n: Int): M[Pure, Boolean] =
      val E = summon[Effects[M]]
      if n == 0 then E.pure(true) else E.tailcall(isOdd[M](n - 1))
    def isOdd[M[_[+_], _] : Effects](n: Int): M[Pure, Boolean] =
      val E = summon[Effects[M]]
      if n == 0 then E.pure(false) else E.tailcall(isEven[M](n - 1))
    assertEquals(Effects[Prog].run(isEven[Prog](1000000)), true)
    assertEquals(Effects[Prog].run(isOdd[Prog](1000000)), false)

  test("foldCont at the machine's own carrier: a multi-shot handler through `control` — every choice, in order"):
    enum Choose[+A]:
      case Flip extends Choose[Boolean]
    val E = Effects[Prog]
    val p: Prog[Choose, Int] =
      E.perform(Choose.Flip).flatMap(a => E.perform(Choose.Flip).map(b => (if a then 2 else 0) + (if b then 1 else 0)))
    val all: Carrier[Int, List[Int], List[Int]] =
      p.foldCont[List[Int]]([X] => (op: Choose[X]) => op match
        case Choose.Flip => E.control.shift[X, List[Int], List[Int]](k => k(true) ++ k(false)))
    assertEquals(E.control./(E.control.map(all)((n: Int) => List(n)))(identity), List(3, 2, 1, 0))

  test("100 000 operations folded in constant stack; runWith is foldCont / identity"):
    val E = Effects[Prog]
    def loop(n: Int, acc: Int): Prog[Produce, Int] =
      if n == 0 then E.pure(acc) else E.perform[Produce, Int](1).flatMap(x => loop(n - 1, acc + x))
    val folded: Carrier[Int, Int, Int] = loop(100000, 0).foldCont[Int]([X] => (op: Produce[X]) => E.control.pure(op))
    assertEquals(E.control./(folded)(identity), 100000)
    assertEquals(E.control./(loop(1000, 0).foldCont(okay.interpr[Carrier, Produce, Int]))(identity), loop(1000, 0).runWith)

  test("the interface's own handle, on the machine: an abort through `control.shift`, the rest forwarded"):
    enum Stop[+A] derives okay.Effect:
      case Now extends Stop[Nothing]
    val E = Effects[Prog]
    val p: Prog[Stop + Produce, Int] =
      E.perform[Stop + Produce, Int](1).flatMap(x => if x > 0 then E.perform[Stop + Produce, Int](Stop.Now) else E.pure(x))
    val handled: Prog[Produce, Option[Int]] =
      E.handle[Stop, Produce][Int, Option[Int]](p)(a => E.pure(Some(a)))([X] => (_: Stop[X]) => E.control.shift(_ => E.pure(None)))
    assertEquals(handled.runWith, None)

  test("reify and reflect: a Prog as a Free tree and back"):
    val tree: Int ! Produce = okay.reify[Prog, Produce, Int](prog[Prog])
    assertEquals(tree.runWith, 3)
    assertEquals(okay.reflect[Prog, Produce, Int](tree).runWith, 3)
