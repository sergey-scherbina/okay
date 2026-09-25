package okay

import Direct.*

/**
 * The glyph is the direct block's mark again (specs/unwrap-glyph.md,
 * stage 3). It could not be while two other things answered `?` on a
 * program: `Throws.?`, which through the `into` conversion answered it
 * on ANY value and did nothing, and the row peek — now `peek`, a word
 * for a method that RUNS operations.
 */
class TestUnwrapMark extends munit.FunSuite {

  test("the glyph is the direct mark again, and the spellings agree") {
    val log = collection.mutable.Buffer.empty[String]
    given Handler[Reader % Int] = new:
      def handle[A](e: Reader[Int, A]): A = e match
        case Reader.Ask() => log += "ask"; 21

    def viaGlyph: Int ! Reader % Int = Direct.direct(Reader.ask[Int].? * 2)
    def viaPrefix: Int ! Reader % Int = Direct.direct(!Reader.ask[Int] * 2)
    def viaWord: Int ! Reader % Int = Direct.direct(Reader.ask[Int].reflect * 2)

    assertEquals(viaGlyph.runWith, 42)
    assertEquals(viaPrefix.runWith, 42)
    assertEquals(viaWord.runWith, 42)
    // the same operations, in the same order, for all three
    assertEquals(log.toList, List("ask", "ask", "ask"))
  }

  // mark-glyph-only (stage 5): `.!?` was a second postfix spelling of
  // the same mark, kept only because it was already written; the
  // glyph is `.?` and nothing else answers the postfix position.
  test("the retired .!? spelling no longer compiles") {
    assert(compileErrors("Direct.direct(Reader.ask[Int].!? * 2)").nonEmpty)
    assert(compileErrors("Direct.generator[Int] { Gen.emit(1).!? }").nonEmpty)
  }

}
