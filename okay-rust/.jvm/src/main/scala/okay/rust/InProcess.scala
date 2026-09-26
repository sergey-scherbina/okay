package okay.rust

import java.nio.file.{Files, Path}
import okay.codec.{WireAuth, WireCompression, WireDeadline, WireFormat}
import okay.foreign.ForeignWorker

/**
 * A worker IN THIS PROCESS in one line (polyglot-one-wire, stage 3):
 *
 * {{{
 * import okay.rust.*
 * val w = ForeignWorker.inProcess(dylib)          // a Rust cdylib, through FFM
 * val v = ForeignWorker.inProcessWasm(module)     // Rust or Go as WebAssembly
 * }}}
 *
 * They are `over(InProcessLinks.ffm(...))` and `over(InProcessLinks.wasm(...))`
 * with the loading and its refusals done for you: a library that is not an
 * okay worker (no `okay_exchange`) is refused by name, and closed.
 */
extension (worker: ForeignWorker.type)

  /** a Rust `cdylib` built with `okay::export_worker!`, loaded through FFM */
  def inProcess(library: Path)(using WireFormat, WireCompression, WireAuth, WireDeadline, CDataCodec): ForeignWorker =
    val lib = NativeLib.load(library)
    InProcessLinks.ffm(lib) match
      case Right(link) => worker.over(link, s"the library $library")
      case Left(why) =>
        lib.close()
        throw IllegalStateException(s"$library is not an okay worker library (built with okay::export_worker!): $why")

  /** a Rust or Go module compiled to WebAssembly (`okay::export_worker!`,
   * `okay.Export`), run under Chicory */
  def inProcessWasm(module: Path)(using WireFormat, WireCompression, WireAuth, WireDeadline): ForeignWorker =
    worker.over(InProcessLinks.wasm(WasmLib.load(Files.readAllBytes(module))), s"the module $module")
