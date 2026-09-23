package okay.js

import java.nio.file.Files
import okay.js.Dyn.global

/** typescript-types T8 against a LIVE tsc: the printed TypeScript compiles, and its types bind */
class TestDirectTsc extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !has("tsc", "--version")

  private def tsc(source: String): (Boolean, String) =
    val dir = Files.createTempDirectory("okay-js-tsc")
    Files.writeString(dir.resolve("page.ts"), source): Unit
    val p = ProcessBuilder("tsc", "--noEmit", "--strict", "--target", "es2022", "--lib", "es2022,dom", "page.ts")
      .directory(dir.toFile).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes(), "UTF-8")
    (p.waitFor() == 0, out)

  test("tsc --strict accepts what Direct.tsSource printed") {
    val src = Direct.tsSource {
      val n = 3
      val label = "items: "
      val show = (k: Int) => global.console.log(label + k * n)
      if (n > 2) { global.setTimeout(show, 10) }
      global.document.title = label
    }
    val (ok, out) = tsc(src)
    assert(ok, s"$out\n$src")
  }

  test("the annotations bind: a value of the wrong type under a Scala type is refused") {
    val wrong = Js.printTs(Vector(Stmt.TypedVar("n", "number", Js.Str("three"))))
    val (ok, out) = tsc(wrong)
    assert(!ok, wrong)
    assert(out.contains("not assignable to type 'number'"), out)
  }
