package okay2

/** TestShiftStack's twin on Native, which switches stacks as the JVM does (TestContStackNative's reason) */
class TestShiftStackNative extends munit.FunSuite {
  test("depth: 100 000 nested resets of one answer type, past the stack's room") {
    def nest(n: Int): Int ! Pure =
      if (n == 0) pure[Pure, Int](0)
      else reset[Int, Pure](pure[Shift[Int], Unit](()).flatMap(_ => nest(n - 1).plus[Shift[Int]])
        .flatMap(x => shift0[Int, Int, Pure](k => k(x + 1))))
    assertEquals(!.run(nest(100000)), 100000)
  }
}
