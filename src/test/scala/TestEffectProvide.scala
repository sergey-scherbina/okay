package okay

import okay.Direct.{*, given}
import scala.language.implicitConversions

/**
 * A signature that declares the row-split test and NOT auto-coloring:
 * `derives TypeableK`, which is the escape hatch `Effects.scala` names
 * for exactly this ("An effect that wants the row-split test and NOT
 * auto-coloring writes `derives TypeableK` instead").
 *
 * It exists because the suite below is about `provide` and
 * `providing`, and those can only be TESTED on a signature that does
 * not already grant itself the permission. `Reader`, which this file
 * used before 2026-09-09, derives `okay.Effect` — and since
 * `Effect extends Direct.Effect` every one of its blocks colors with
 * no grant at all, so the two positive tests below would have passed
 * with `provide` deleted from them.
 */
enum ProvideProbe[+A] derives TypeableK:
  case Ask() extends ProvideProbe[Int]

object ProvideProbe:
  /** typed at the SIGNATURE, not at `!`: `opColor` converts `G[A]` to
   * `A`, so a raw case constructor is too precise for G to infer */
  def ask: ProvideProbe[Int] = Ask()

  given handler: Handler[ProvideProbe] with
    def handle[A](a: ProvideProbe[A]): A = a match
      case Ask() => 41

/**
 * Coloring as POLICY (specs/direct-auto-coloring.md): Direct.Effect[G]
 * markers are ordinary givens, so provide/providing install them
 * per scope — a block's code auto-colors only where its caller
 * granted the permission.
 *
 * WHAT CHANGED UNDER THIS FILE, and why it is rewritten (2026-09-09,
 * grant-unenforced). `okay.Effect` became
 * `trait Effect[F[_]] extends TypeableK[F], Direct.Effect[F]`
 * (2026-09-08, the operator's call): declaring a signature with
 * `derives Effect` now also grants its operations the coloring
 * permission. That is deliberate and documented in `Effects.scala`.
 *
 * It also made this file's subject untestable on `Reader`. The
 * negative test went red on master — the ungranted block compiled —
 * and it was RIGHT to: `Reader derives okay.Effect`, so its block no
 * longer needs a grant and never will again. The two positive tests
 * did not go red, which is worse: they had become vacuous, passing
 * whether or not `provide` did anything at all.
 *
 * So the suite moved to `ProvideProbe`, which declares itself with
 * `derives TypeableK` and therefore still REQUIRES the grant. The
 * last test pins the new rule from the other side, so that a future
 * reader meets it as a decision rather than as a leak.
 */
class TestEffectProvide extends munit.FunSuite {

  /** the block REQUIRES the coloring permission from its scope */
  def prog(using Direct.Effect[ProvideProbe]): Int ! ProvideProbe = direct {
    val env: Int = ProvideProbe.ask       // colors by the grant
    env + 1
  }

  val grant: Direct.Effect[ProvideProbe] = new Direct.Effect[ProvideProbe] {}

  test("provide grants the coloring policy for one expression") {
    assertEquals(provide(grant)(prog).runWith, 42)
  }

  test("providing composes the policy as a layer") {
    val base = providing[Direct.Effect[ProvideProbe]](grant)
    assertEquals(base { prog }.runWith, 42)
  }

  test("without the grant the same block does not color") {
    val e = compileErrors(
      "import okay.Direct.{*, given}; import scala.language.implicitConversions; " +
        "okay.Direct.direct[[X] =>> X ! okay.ProvideProbe] " +
        "{ val env: Int = okay.ProvideProbe.ask; env } ")
    // the MESSAGE, not merely `nonEmpty`: a negative test that accepts
    // any error at all passes when the block breaks for an unrelated
    // reason, and then it is no longer testing what it says.
    //
    // What the refusal looks like is worth knowing: NOT the
    // `@implicitNotFound` on `Direct.Effect`, which is what I first
    // asserted and which never appears here. A missing CONVERSION is
    // not reported as a missing implicit — the compiler simply finds
    // no way from `ProvideProbe[Int]` to `Int` and says so as a type
    // mismatch. That mismatch IS the rule holding: the operation
    // stayed the operation, because nothing granted it the right to
    // color. (`TestErrorMessages` sees the implicitNotFound text
    // because it summons the marker directly.)
    assert(e.contains("okay.ProvideProbe[Int]") && e.contains("Required: Int"),
      s"the ungranted operation must stay uncolored, got: $e")
  }

  test("a signature that declares itself an effect needs no grant (2026-09-08)") {
    // the other side of the same rule, pinned: `Reader derives
    // okay.Effect`, and `Effect extends Direct.Effect`, so its
    // operations color with nothing provided. This is what made the
    // old version of this file's negative test fail, and it is
    // intended — `derives Effect` is the signature author's own
    // declaration that its values ARE operations.
    def rask: Reader[Int, Int] = Reader.Ask()
    val p: Int ! (Reader % Int) = direct {
      val env: Int = rask
      env + 1
    }
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](41)(p)), 42)
  }
}
