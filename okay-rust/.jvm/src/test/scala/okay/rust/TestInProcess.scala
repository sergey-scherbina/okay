package okay.rust

import java.nio.file.{Files, Path}
import okay.py.{ForeignWorker, RustWorker, RustWorkerBinary, Rs, Foreign, WireConformance}

/** the conformance crate as a LIBRARY: the same programs and functions as the
 * Rust worker binary, exported in-process by `okay::export_worker!` */
object RustInProcess:
  private def has = scala.util.Try(ProcessBuilder("cargo", "--version").start().waitFor() == 0).getOrElse(false)
  lazy val available: Boolean = has

  private def crate(): Path =
    val dir = Files.createTempDirectory("okay-rust-inprocess")
    Files.createDirectories(dir.resolve("src")): Unit
    Files.writeString(dir.resolve("src").resolve("ops.rs"),
      Rs.ops(Foreign.callbacks(RustWorkerBinary.priceOf, RustWorkerBinary.discount))): Unit
    // the binary's source, with its `fn main` replaced by the export
    // (no margin: the docs quote the export line)
    val exported = """
okay::export_worker!(make);
"""
    val lib = RustWorkerBinary.main.replace("fn main() {\n    okay::main(make)\n}\n", exported)
    Files.writeString(dir.resolve("src").resolve("lib.rs"), lib): Unit
    dir

  lazy val dylib: Path = RustWorker.buildLibrary(crate())

  /** a cdylib that is NOT an okay worker: the Argon2 kernel, which exports
   * okay_argon2id and no okay_exchange */
  lazy val notAWorker: Path =
    val target = Files.createTempDirectory("okay-rust-target")
    val p = ProcessBuilder("cargo", "build", "--offline", "--release", "--target-dir", target.toString)
      .directory(Kernels.dir("argon2").toFile).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(said)
    target.resolve("release").resolve(NativeLib.fileName("okay_argon2"))
  lazy val wasm: Path = RustWorker.buildLibrary(crate(), Some("wasm32-wasip1"))

/** (Rust, FFM): the worker IN THIS PROCESS, a cdylib called through FFM */
class TestRustFfm extends WireConformance:
  override def munitIgnore: Boolean = !RustInProcess.available
  lazy val engine: ForeignWorker =
    ForeignWorker.inProcess(RustInProcess.dylib)
  override def afterAll(): Unit = if RustInProcess.available then engine.close()

  test("in-process, the default does not compress: a message here is a memory copy, and a table crosses in place") {
    assertEquals(engine.wire, "json/none+cdata")
  }

  test("a library that is not an okay worker is refused by name") {
    val e = intercept[IllegalStateException](ForeignWorker.inProcess(RustInProcess.notAWorker))
    assert(e.getMessage.contains("is not an okay worker library (built with okay::export_worker!)"), e.getMessage)
    assert(e.getMessage.contains("okay_exchange"), e.getMessage)
  }

/** (Rust, WebAssembly): the same crate as wasm32-wasip1, under Chicory. No
 * threads in wasip1, so no direct style: programs as data only */
class TestRustWasm extends WireConformance:
  override def munitIgnore: Boolean = !RustInProcess.available
  override def direct: Boolean = false
  override def tables: Boolean = false
  // wasm32-wasip1 builds with panic=abort: a panic traps the module
  override def survivesPanics: Boolean = false
  // one instance for the suite: the panic test, which leaves it dead, is the
  // last of WireConformance's tests to run here (the one after it is skipped)
  lazy val engine: ForeignWorker =
    ForeignWorker.inProcessWasm(RustInProcess.wasm)

/** (Go, WebAssembly): the Go worker compiled to wasip1, in this process under Chicory */
class TestGoWasm extends WireConformance:
  override def munitIgnore: Boolean = !okay.py.GoWorkerBinary.available
  lazy val engine: ForeignWorker =
    ForeignWorker.inProcessWasm(GoInProcess.wasm)

object GoInProcess:
  lazy val wasm: Path =
    val dir = Files.createTempDirectory("okay-go-inprocess")
    Files.createDirectories(dir.resolve("shop")): Unit
    Files.writeString(dir.resolve("shop").resolve("ops.go"),
      okay.py.Go.ops("shop", Foreign.callbacks(okay.py.TestGoProgram.priceOf, okay.py.TestGoProgram.discount))): Unit
    Files.writeString(dir.resolve("main.go"), okay.py.TestGoProgram.main): Unit
    okay.py.GoWorker.buildWasm(dir)

/** (Go, WebAssembly), CBOR and DEFLATE chosen by givens */
class TestGoWasmCbor extends WireConformance:
  import okay.py.WireFormat.Cbor.given
  import okay.py.WireCompression.Deflate.given
  override def munitIgnore: Boolean = !okay.py.GoWorkerBinary.available
  lazy val engine: ForeignWorker =
    ForeignWorker.inProcessWasm(GoInProcess.wasm)

/** (Rust, FFM), CBOR and DEFLATE */
class TestRustFfmCbor extends WireConformance:
  import okay.py.WireFormat.Cbor.given
  import okay.py.WireCompression.Deflate.given
  override def munitIgnore: Boolean = !RustInProcess.available
  lazy val engine: ForeignWorker =
    ForeignWorker.inProcess(RustInProcess.dylib)
  override def afterAll(): Unit = if RustInProcess.available then engine.close()

/** (Rust, WebAssembly), CBOR and DEFLATE */
class TestRustWasmCbor extends WireConformance:
  import okay.py.WireFormat.Cbor.given
  import okay.py.WireCompression.Deflate.given
  override def munitIgnore: Boolean = !RustInProcess.available
  override def direct: Boolean = false
  override def tables: Boolean = false
  override def survivesPanics: Boolean = false
  lazy val engine: ForeignWorker =
    ForeignWorker.inProcessWasm(RustInProcess.wasm)
