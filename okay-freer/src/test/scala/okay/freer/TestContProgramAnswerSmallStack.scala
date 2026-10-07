package okay.freer


/** cont-program-answer on a 128 KB JVM thread: the strict leaf nests a run per level and needs a stack switch
 * there; the lazy `k` holds no host frame per level, forced or stepped into */
class TestContProgramAnswerSmallStack extends munit.FunSuite:
  import ContProgramAnswer.*
  val n = 1000000

  test("a million nested program-answered bodies on 128 KB: forced by the Free fold") {
    assertEquals(SmallStack.run(128)(!.run(pureAns(n, Cps.programLeaf[Int, Ans, Ans]))), n)
  }

  test("a million nested on 128 KB, consumed by a running machine that steps in") {
    assertEquals(SmallStack.run(128)(!.run(Shift.run[Int, Pure](dynAns(n)))), n)
  }
