package okay.rust

import java.nio.file.{Files, Path}
import okay.{Handler, given}

/** polyglot-rust stage 3 against a LIVE cargo with the wasm32-wasip1 target: the kernel under Chicory */
class TestPasswordHashWasm extends munit.FunSuite {
  import TestPasswordHash.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def run(cmd: String*): (Boolean, String) =
    scala.util.Try {
      val p = ProcessBuilder(cmd*).redirectErrorStream(true).start()
      val said = String(p.getInputStream.readAllBytes())
      (p.waitFor() == 0, said)
    }.getOrElse((false, ""))
  override def munitIgnore: Boolean =
    !run("cargo", "--version")._1 || !run("rustup", "target", "list", "--installed")._2.contains("wasm32-wasip1")

  private def crate: Path = Kernels.dir("argon2")

  /** the same crate, built OFFLINE for wasm32-wasip1 */
  private lazy val lib: WasmLib =
    val target = Files.createTempDirectory("okay-rust-wasm")
    val p = ProcessBuilder("cargo", "build", "--offline", "--release", "--target", "wasm32-wasip1",
      "--target-dir", target.toString).directory(crate.toFile).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    assert(p.waitFor() == 0, said)
    WasmLib.load(Files.readAllBytes(target.resolve("wasm32-wasip1/release/okay_argon2.wasm")))

  private def wasm: Handler[PasswordHash] = PasswordHash.wasm(lib)

  test("THE LAW, as WebAssembly: the kernel's bytes under Chicory are BouncyCastle's") {
    val cases = for
      (m, t, p) <- Vector((8, 1, 1), (64, 2, 1), (256, 3, 2), (1024, 1, 4))
      salt <- Vector("saltsalt", "a longer salt, sixteen+")
      password <- Vector("", "correct horse battery staple")
      length <- Vector(16, 32, 64)
    yield new PasswordHash.Argon2id(password.getBytes("UTF-8"), salt.getBytes("UTF-8"), m, t, p, length)
    val differ = cases.filter { op =>
      PasswordHash.argon2id(op.password, op.salt, op.memoryKb, op.iterations, op.parallelism, op.length)
        .runWith(using wasm).map(hex) != bouncy(op).map(hex)
    }
    assertEquals(differ.map(op => (op.memoryKb, op.iterations, op.parallelism, op.length)), Vector.empty)
  }

  test("one program, the third handler: the same answer as the JVM's") {
    assertEquals(stored("pw", "saltsalt").runWith(using wasm), stored("pw", "saltsalt").runWith(using PasswordHash.using(bouncy)))
  }

  test("a refused parameter set is the same Left as the native road's") {
    val refused = PasswordHash.argon2id("pw".getBytes, "saltsalt".getBytes, memoryKb = 1, iterations = 1, parallelism = 1)
      .runWith(using wasm)
    assertEquals(refused, Left("okay_argon2id answered -2: parameters Argon2 refuses"))
  }

  test("a function the module does not export is refused by name") {
    assertEquals(lib.call("okay_argon3"), Left("the module exports no function `okay_argon3`"))
  }
}
