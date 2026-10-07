package okay.freer

import okay.std.*
import okay.std.given
/** stage 2's depth programs on a 128 KB JVM thread: a machine that held a
 * host frame per level would overflow here at a few thousand */
class TestDelimitedDepthSmallStack extends munit.FunSuite:
  import DelimitedDepth.*

  val M = LambdaDollar.machine
  val n = 1000000

  test("all four, a million deep, on 128 KB") {
    assertEquals(SmallStack.run(128)(M.run(answerUsing(M)(n))), 2 * n)
    assertEquals(SmallStack.run(128)(M.run(nestedResets(M)(n))), n)
    assertEquals(SmallStack.run(128)(M.run(leftBinds(M)(n))), n)
    assertEquals(SmallStack.run(128)(M.run(multiShot(M)(20))), 1 << 20)
  }
