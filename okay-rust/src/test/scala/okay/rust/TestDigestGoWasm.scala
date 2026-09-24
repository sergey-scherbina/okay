package okay.rust

import java.nio.file.{Files, Path, Paths}
import okay.{!, Handler, given}

/** polyglot-go stage 2 against a LIVE Go toolchain: a Go plugin as WebAssembly, under Chicory */
class TestDigestGoWasm extends munit.FunSuite {
  import TestKdf.hex

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val go = scala.util.Try(ProcessBuilder("go", "version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !go

  private def plugin: Path =
    val here = Paths.get("kernels/sha256-go")
    if Files.exists(here.resolve("go.mod")) then here else Paths.get("okay-rust/kernels/sha256-go")

  /** built OFFLINE: GOOS=wasip1 GOARCH=wasm, a reactor module, no TinyGo */
  private lazy val module: Array[Byte] =
    val out = Files.createTempDirectory("okay-go-wasm").resolve("plugin.wasm")
    val pb = ProcessBuilder("go", "build", "-buildmode=c-shared", "-o", out.toString, ".")
      .directory(plugin.toFile).redirectErrorStream(true)
    pb.environment().put("GOOS", "wasip1")
    pb.environment().put("GOARCH", "wasm")
    pb.environment().put("GOTOOLCHAIN", "local")
    val p = pb.start()
    val said = String(p.getInputStream.readAllBytes())
    assert(p.waitFor() == 0, said)
    Files.readAllBytes(out)

  private lazy val lib: WasmLib = WasmLib.load(module)

  /** a program written against the effect, knowing nothing of Go */
  private def fingerprint(text: String): Either[String, String] ! Digest =
    Digest.sha256(text.getBytes("UTF-8")).map(_.map(hex))

  test("THE LAW: the Go plugin's SHA-256, under Chicory, is the JDK's") {
    given Handler[Digest] = Digest.wasm(lib)
    val rnd = scala.util.Random(24)
    val sizes = (0 to 130) ++ Vector(1000, 4096, 65536)
    val differ = sizes.filter { n =>
      val bytes = Array.fill(n)(rnd.nextInt(256).toByte)
      Digest.sha256(bytes).runWith.map(hex) != Right(hex(java.security.MessageDigest.getInstance("SHA-256").digest(bytes)))
    }
    assertEquals(differ, Vector.empty)
  }

  test("one program, either handler: the same fingerprint from Go and from the JDK") {
    assertEquals(fingerprint("okay").runWith(using Digest.wasm(lib)), fingerprint("okay").runWith(using Digest.jdk))
    assertEquals(fingerprint("").runWith(using Digest.jdk),
      Right("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  }

  test("a Go panic is a Left carrying the plugin's own message, not a bare trap") {
    // an instance of its own: after a panic the plugin's runtime is gone
    val dying = WasmLib.load(module)
    val r = dying.call("okay_panic")
    assert(r.left.exists(m => m.contains("`okay_panic` trapped") && m.contains("the go plugin says no")), r)
  }
}
