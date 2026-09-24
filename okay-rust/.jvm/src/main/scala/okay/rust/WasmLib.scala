package okay.rust

import com.dylibso.chicory.runtime.{ImportValues, Instance}
import com.dylibso.chicory.wasi.{WasiOptions, WasiPreview1}
import com.dylibso.chicory.wasm.Parser

/**
 * A WebAssembly module run by Chicory, a WebAssembly runtime written in
 * Java (specs/polyglot-rust.md, stage 3): the same kernel as the native
 * road, with NO native code in the process. The module's memory is its own
 * linear memory, so a bug in the kernel cannot reach the JVM's heap — the
 * road for untrusted plugins, and for Go (`GOOS=wasip1`) as well as Rust.
 *
 * The WASI it is given grants NOTHING: no files, no environment, no
 * arguments. A module compiled for `wasm32-wasip1` imports a few WASI
 * functions (writing a panic's message, reading its environment); here they
 * find an empty world.
 *
 * A host cannot hand a module its own pointers, so a buffer is the MODULE's
 * memory: `okay_alloc(n)` answers where, the host writes there, and
 * `okay_free(p, n)` gives it back. `withBuffers` frees every buffer it made,
 * whatever the call did.
 */
final class WasmLib private (instance: Instance, said: java.io.ByteArrayOutputStream):

  /** what the module wrote to its stderr (a panic's message), and forget it */
  def stderr(): String = said.synchronized { val s = said.toString("UTF-8"); said.reset(); s }

  /**
   * Call the exported function `name`. A missing export is a Left, by name,
   * and so is a TRAP: the module's own fault, with what it wrote to its
   * stderr just before — a Go or Rust panic prints its reason there, and
   * the trap alone says only "unreachable".
   */
  def call(name: String, args: Long*): Either[String, Long] =
    scala.util.Try(instance.`export`(name)).toOption.filter(_ != null) match
      case None => Left(s"the module exports no function `$name`")
      case Some(f) =>
        try
          val out = f.apply(args*)
          if out == null || out.isEmpty then Right(0L) else Right(out(0))
        catch case e: com.dylibso.chicory.wasm.ChicoryException =>
          val why = stderr().trim
          Left(s"`$name` trapped: ${e.getMessage}${if why.isEmpty then "" else s" — the module said: $why"}")

  /** run `body` with a place to put buffers; every one is freed after it */
  def withBuffers[A](body: WasmLib.Buffers => A): A =
    val made = scala.collection.mutable.ArrayBuffer.empty[(Long, Long)]
    val buffers = new WasmLib.Buffers:
      def in(bytes: Array[Byte]): Long =
        val p = alloc(bytes.length.toLong)
        made += ((p, bytes.length.toLong))
        if bytes.nonEmpty then instance.memory().write(p.toInt, bytes)
        p
      def out(n: Int): Long =
        val p = alloc(n.toLong)
        made += ((p, n.toLong))
        p
      def read(p: Long, n: Int): Array[Byte] = instance.memory().readBytes(p.toInt, n)
    try body(buffers)
    finally made.foreach((p, n) => call("okay_free", p, n): Unit)

  private def alloc(n: Long): Long =
    call("okay_alloc", math.max(1L, n)).fold(why => throw IllegalStateException(why), identity)

object WasmLib:

  /** buffers in a module's memory, during one `withBuffers` */
  trait Buffers:
    /** a copy of `bytes`, where the module can read it */
    def in(bytes: Array[Byte]): Long
    /** `n` bytes for the module to write */
    def out(n: Int): Long
    /** what the module wrote at `p` */
    def read(p: Long, n: Int): Array[Byte]

  /** instantiate a `.wasm` module under a WASI that grants nothing: its
   * stderr goes to a buffer of its own, never to this process's */
  def load(bytes: Array[Byte]): WasmLib =
    val said = java.io.ByteArrayOutputStream()
    val wasi = WasiPreview1.builder().withOptions(WasiOptions.builder().withStderr(said).build()).build()
    val imports = ImportValues.builder().addFunction(wasi.toHostFunctions()*).build()
    val lib = new WasmLib(Instance.builder(Parser.parse(bytes)).withImportValues(imports).build(), said)
    // A REACTOR module (a Go `-buildmode=c-shared` build, a Rust cdylib)
    // exports `_initialize`, which the WASI reactor convention says to call
    // once before any other export. Chicory's `withInitialize` is about the
    // module's own segments and does not call it — found by Go's runtime,
    // whose first export trapped with "wasmexport function called before
    // runtime initialization" (go-wasm, 2026-09-24).
    lib.call("_initialize") match
      case Left(why) if !why.startsWith("the module exports no function") => throw IllegalStateException(why)
      case _ => ()
    lib
