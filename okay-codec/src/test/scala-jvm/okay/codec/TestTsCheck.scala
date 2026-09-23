package okay.codec

import TestStubs.*

/** typescript-types T4 against a LIVE tsc: the same types, or which differ */
class TestTsCheck extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val tsc = scala.util.Try(ProcessBuilder("tsc", "--version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !tsc

  private def generated = Stubs.typescript(summon[Schema[Line]], summon[Schema[Shape]])

  // a TypeScript developer's own copy: other order, other formatting, the same types
  private val handwritten = """
export type Shape = { Rect: Rect } | { Circle: Circle };
export interface Circle { r: number }
export interface Rect { h: number; w: number }
export interface Line { price: number; qty: number; sku: string }
"""

  test("a hand-written copy in another order and format IS the same types") {
    assertEquals(TsCheck.same(generated, handwritten), Right(()))
  }

  test("a renamed field is found, by the type's name") {
    val got = TsCheck.same(generated, handwritten.replace("qty: number", "quantity: number"))
    assert(got.left.exists(_.exists(_.startsWith("Line:"))), got.toString)
  }

  test("a field made optional is a different type too") {
    val got = TsCheck.same(generated, handwritten.replace("r: number", "r?: number"))
    assert(got.left.exists(m => m.exists(_.startsWith("Circle:")) || m.exists(_.startsWith("Shape:"))), got.toString)
  }

  test("a type the hand-written copy lacks is reported by its name") {
    val got = TsCheck.same(generated, handwritten.replace("export interface Line { price: number; qty: number; sku: string }", ""))
    assert(got.left.exists(_.exists(_.startsWith("Line:"))), got.toString)
  }
}
