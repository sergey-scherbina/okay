package okay.rust

import java.lang.foreign.{Arena, FunctionDescriptor, Linker, SymbolLookup}
import java.lang.invoke.MethodHandle
import java.nio.file.Path

/**
 * A native library over the C ABI, bound through FFM (JEP 454, final in
 * JDK 22; specs/polyglot-rust.md): what a Rust `cdylib` exports, with no
 * JNI glue and no generated code. The library stays loaded until `close`.
 *
 * A symbol it does not export is refused by NAME, as a `Left`, rather than
 * as an exception at the first call.
 */
final class NativeLib private (val path: Path, arena: Arena, lookup: SymbolLookup) extends AutoCloseable:

  /** the function `name`, called with `descriptor`'s layouts */
  def function(name: String, descriptor: FunctionDescriptor): Either[String, MethodHandle] =
    val found = lookup.find(name)
    if found.isPresent then Right(Linker.nativeLinker().downcallHandle(found.get, descriptor))
    else Left(s"$path exports no symbol `$name`")

  def close(): Unit = arena.close()

object NativeLib:
  /** load the library at `path`: a `.dylib`, a `.so` or a `.dll` */
  def load(path: Path): NativeLib =
    val arena = Arena.ofShared()
    new NativeLib(path, arena, SymbolLookup.libraryLookup(path, arena))

  /** the file a Cargo `cdylib` named `crate` builds to on this OS */
  def fileName(crate: String): String =
    val os = System.getProperty("os.name").toLowerCase
    if os.contains("mac") then s"lib$crate.dylib"
    else if os.contains("win") then s"$crate.dll"
    else s"lib$crate.so"
