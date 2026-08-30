package okay.kyo

import okay.{!, async}
import okay.given
import KyoInterop.*
import _root_.kyo.{<, AllowUnsafe, Duration, KyoApp}

class TestKyoInterop extends munit.FunSuite {

  test("pure kyo evaluates into okay") {
    val k: Int < Any = (1: Int < Any).flatMap((x: Int) => x + 41)
    assertEquals(!.run(fromKyo(k)), 42)
  }

  test("kyo async runs inside one okay operation") {
    val k: Int < _root_.kyo.Async = _root_.kyo.Async.run(21).flatMap(f => f.get).flatMap((x: Int) => x * 2)
    assertEquals(fromKyoAsync(k).runWith, 42)
  }

  test("okay async becomes a kyo suspension") {
    import AllowUnsafe.embrace.danger
    val k = toKyo(async(40).map(_ + 2))
    assertEquals(KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow, 42)
  }
}
