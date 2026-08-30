package okay.kyo

import okay.{!, Async, Pure, async, pure}
import okay.given
import _root_.kyo.{<, Abort, AllowUnsafe, Duration, Flat, KyoApp}

/**
 * Interop with kyo (specs/interop.md): value and Async bridges. A
 * pure kyo computation evaluates into a pure okay program; a kyo
 * async computation runs to completion inside one okay Async
 * operation (blocking a virtual thread, Loom-style); an okay Async
 * program becomes a kyo value the blunt way — kyo's effect rows are
 * ArrowEffects internally, so a structural embedding is out of scope
 * here (the shared-subset row mapping is future work).
 */
object KyoInterop {

  /** a pure kyo computation as a pure okay program */
  def fromKyo[A: Flat](k: => A < Any): A ! Pure = pure(k.eval)

  /** run a kyo async computation inside one okay Async operation */
  def fromKyoAsync[A: Flat](k: => A < (Abort[Nothing] & _root_.kyo.Async)): A ! Async =
    async {
      import AllowUnsafe.embrace.danger
      KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow
    }

  /** an okay Async program as a kyo IO suspension */
  def toKyo[A](p: => A ! Async): A < _root_.kyo.IO =
    _root_.kyo.IO(p.runWith)
}
