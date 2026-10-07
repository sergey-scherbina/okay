package okay.kyo

import okay.freer.!
import okay.given
import okay.freer.given
import okay.std.given
import _root_.kyo.{<, Abort, Async as KAsync}

/** kyo's pending type as an effect of the tree (specs/foreign-effects-in-tree.md, stage 1) */
class TestKyoMembers extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  test("a kyo value kept in the tree, lowered to okay's Async by KyoEffect.run") {
    val k: Int < (Abort[Nothing] & KAsync) = 20
    val p: Int ! <[*, Abort[Nothing] & KAsync] =
      KyoEffect.perform(k).flatMap(a => KyoEffect.perform[Int, Abort[Nothing] & KAsync](a + 1))
    assertEquals(KyoEffect.run(p).runWith, 21)
  }
}
