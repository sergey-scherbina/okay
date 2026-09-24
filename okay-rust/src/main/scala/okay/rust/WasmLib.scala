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
final class WasmLib private (instance: Instance):

  /** call the exported function `name`; a missing export is a Left, by name */
  def call(name: String, args: Long*): Either[String, Long] =
    scala.util.Try(instance.`export`(name)).toOption.filter(_ != null) match
      case None => Left(s"the module exports no function `$name`")
      case Some(f) =>
        val out = f.apply(args*)
        if out == null || out.isEmpty then Right(0L) else Right(out(0))

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

  /** instantiate a `.wasm` module under a WASI that grants nothing */
  def load(bytes: Array[Byte]): WasmLib =
    val wasi = WasiPreview1.builder().withOptions(WasiOptions.builder().build()).build()
    val imports = ImportValues.builder().addFunction(wasi.toHostFunctions()*).build()
    new WasmLib(Instance.builder(Parser.parse(bytes)).withImportValues(imports).build())
