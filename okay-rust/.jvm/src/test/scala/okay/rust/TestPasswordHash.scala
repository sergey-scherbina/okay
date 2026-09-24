package okay.rust

import java.nio.file.{Files, Path}
import java.lang.foreign.{FunctionDescriptor, ValueLayout}
import okay.{!, Handler, given}
import org.bouncycastle.crypto.generators.Argon2BytesGenerator
import org.bouncycastle.crypto.params.Argon2Parameters

object TestPasswordHash:
  /** BouncyCastle's Argon2id — the JVM implementation okay-security-argon2 already ships */
  def bouncy(op: PasswordHash.Argon2id): Either[String, Array[Byte]] =
    val params = Argon2Parameters.Builder(Argon2Parameters.ARGON2_id)
      .withVersion(Argon2Parameters.ARGON2_VERSION_13)
      .withMemoryAsKB(op.memoryKb).withIterations(op.iterations).withParallelism(op.parallelism)
      .withSalt(op.salt).build()
    val gen = Argon2BytesGenerator()
    gen.init(params)
    val out = new Array[Byte](op.length)
    gen.generateBytes(op.password, out): Unit
    Right(out)

  def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xff}%02x").mkString

  /** a program written against the effect, knowing nothing of Rust */
  def stored(password: String, salt: String): Either[String, String] ! PasswordHash =
    PasswordHash.argon2id(password.getBytes("UTF-8"), salt.getBytes("UTF-8"), memoryKb = 64, iterations = 2, parallelism = 1)
      .map(_.map(hex))

/** polyglot-rust stage 1: the effect, with a handler that is a plain function (default gate) */
class TestPasswordHash extends munit.FunSuite {
  import TestPasswordHash.*

  test("a program asks PasswordHash for Argon2id; the handler decides what computes it") {
    given Handler[PasswordHash] = PasswordHash.using(op => Right(op.salt.reverse))
    assertEquals(stored("pw", "ab").runWith, Right("6261"))
  }

  test("under the JVM implementation, the program answers Argon2id itself") {
    given Handler[PasswordHash] = PasswordHash.using(bouncy)
    assertEquals(stored("pw", "saltsalt").runWith.map(_.length), Right(64))
  }
}

/** polyglot-rust stage 1 against a LIVE cargo: the Rust kernel through FFM */
class TestPasswordHashRust extends munit.FunSuite {
  import TestPasswordHash.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private def has(cmd: String*) = scala.util.Try(ProcessBuilder(cmd*).start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !has("cargo", "--version")

  /** the crate, from the module's directory or the repository's */
  private def crate: Path = Kernels.dir("argon2")

  /** built OFFLINE from the checked-in Cargo.lock, into a directory of its own */
  private lazy val lib: NativeLib =
    val target = Files.createTempDirectory("okay-rust-target")
    val p = ProcessBuilder("cargo", "build", "--offline", "--release", "--target-dir", target.toString)
      .directory(crate.toFile).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    assert(p.waitFor() == 0, said)
    NativeLib.load(target.resolve("release").resolve(NativeLib.fileName("okay_argon2")))
  override def afterAll(): Unit = if has("cargo", "--version") then lib.close()

  private def rust: Handler[PasswordHash] = PasswordHash.rust(lib).fold(why => fail(why), identity)

  test("THE LAW: the Rust kernel's bytes are BouncyCastle's bytes") {
    val cases = for
      (m, t, p) <- Vector((8, 1, 1), (64, 2, 1), (256, 3, 2), (1024, 1, 4))
      salt <- Vector("saltsalt", "a longer salt, sixteen+")
      password <- Vector("", "correct horse battery staple")
      length <- Vector(16, 32, 64)
    yield new PasswordHash.Argon2id(password.getBytes("UTF-8"), salt.getBytes("UTF-8"), m, t, p, length)
    val differ = cases.filter { op =>
      val native = PasswordHash.argon2id(op.password, op.salt, op.memoryKb, op.iterations, op.parallelism, op.length)
        .runWith(using rust).map(hex)
      native != bouncy(op).map(hex)
    }
    assertEquals(differ.map(op => (op.memoryKb, op.iterations, op.parallelism, op.length)), Vector.empty)
    assertEquals(cases.size, 48)
  }

  test("one program, either handler: the same answer from Rust and from the JVM") {
    assertEquals(stored("pw", "saltsalt").runWith(using rust), stored("pw", "saltsalt").runWith(using PasswordHash.using(bouncy)))
  }

  test("parameters Argon2 refuses are a Left naming the kernel's answer, not an exception") {
    val refused = PasswordHash.argon2id("pw".getBytes, "saltsalt".getBytes, memoryKb = 1, iterations = 1, parallelism = 1)
      .runWith(using rust)
    assertEquals(refused, Left("okay_argon2id answered -2: parameters Argon2 refuses"))
  }

  test("a symbol the library does not export is refused by name") {
    val r = lib.function("okay_argon3", FunctionDescriptor.of(ValueLayout.JAVA_INT))
    assert(r.left.exists(_.contains("exports no symbol `okay_argon3`")), r)
  }
}
