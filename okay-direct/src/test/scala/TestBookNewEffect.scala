package okay


import okay.freer.*


import okay.freer.given
import okay.Direct.*
import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions

/**
 * THE BOOK'S CHAPTER 14, COMPILED (docs/continuations/14-a-new-effect.md).
 *
 * A capture turned into something its users never see as a capture.
 * The point is what the call sites look like: no prompt, no `k`, no
 * mention of Shift beyond the row they were already in.
 */
class TestBookNewEffect extends munit.FunSuite {

  type Row = Shift % ? + Pure

  // ==== the library ================================================

  /**
   * What a budgeted block hands to the code inside it. The answer
   * type is a MEMBER, not a parameter -- the same trick `Prompted`
   * uses -- so a call site can be written against `Budget[?]` and
   * still name the type when it needs to. Without it, `spend` needs
   * the answer type as an explicit argument at every call.
   */
  final class Budget[R] private[TestBookNewEffect]
      (private[TestBookNewEffect] val left: AtomicInteger,
       private[TestBookNewEffect] val orElse: () => R,
       private[TestBookNewEffect] val prompt: Prompt[R]):
    type Res = R

  object Budget:

    /** run `body` with `limit` to spend. Overspend and the block ends
     * with `orElse`, the rest of the body discarded. */
    def within[R, F[+_]](limit: Int)(orElse: => R)
                        (body: Budget[R] ?=> R ! Shift % ? + F)
                        (using Shift.Machine[F], At): R ! F =
      val p = Shift.prompt[R]
      Shift.run(Shift.push(p)(body(using new Budget(AtomicInteger(limit), () => orElse, p))))

    /** the ONLY thing a caller writes. No cast: `b.Res` is the
     * member, so the prompt and the fallback already agree. */
    def spend[F[+_]](n: Int)(using b: Budget[?], at: At): Unit ! Shift % ? + F =
      if b.left.addAndGet(-n) >= 0 then okay.freer.pure(())
      else Shift.abort[b.Res, Unit, F](b.prompt)(b.orElse())

    /** what is left, as an ordinary question — no capture involved */
    def remaining(using b: Budget[?]): Int = b.left.get

  // ==== the call sites =============================================

  def shop(using Budget[String]): String ! Row = direct:
    !Budget.spend[Pure](30)
    !Budget.spend[Pure](30)
    !Budget.spend[Pure](30)
    s"bought three, ${Budget.remaining} left"

  test("inside the budget, the block finishes normally") {
    assertEquals(!.run(Budget.within[String, Pure](100)("over budget")(shop)),
      "bought three, 10 left")
  }

  test("over the budget, the block ends and the rest is discarded") {
    var reached = false
    def greedy(using Budget[String]): String ! Row = direct:
      !Budget.spend[Pure](60)
      !Budget.spend[Pure](60)        // 120 > 100: leaves here
      reached = true
      "never"
    assertEquals(!.run(Budget.within[String, Pure](100)("over budget")(greedy)),
      "over budget")
    assert(!reached, "the code after the overspend ran")
  }

}
