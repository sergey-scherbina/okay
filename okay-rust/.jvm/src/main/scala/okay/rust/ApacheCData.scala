package okay.rust

import java.lang.foreign.MemorySegment
import okay.arrow.{ApacheArrow, Table}
import org.apache.arrow.c.{ArrowArray, ArrowSchema, Data}
import org.apache.arrow.memory.RootAllocator

/**
 * The C Data Interface through the real thing: Apache Arrow Java's
 * `arrow-c-data` (foreign-arrow-ffm), behind an import —
 *
 * {{{
 * import okay.rust.ApacheCData.given
 * }}}
 *
 * An OPTIONAL dependency of okay-rust, as Arrow Java is of okay-arrow: a
 * program that uses this adds `org.apache.arrow:arrow-c-data`,
 * `arrow-vector` and `arrow-memory-unsafe` (19.0.0) and runs with the flags
 * Arrow's memory needs; without them the first use is refused by name. The
 * table crosses through `ApacheArrow.toRoot`/`fromRoot`, so what this
 * exports is what Arrow Java's own vectors hold, and TestCData proves each
 * codec reads the other's structs.
 */
object ApacheCData extends CDataCodec:

  def name = "apache"

  given codec: CDataCodec = this

  /** why this cannot run here, or None */
  def missing: Option[String] =
    ApacheArrow.missing().orElse(ApacheArrow.missing("org.apache.arrow.c.Data")).map(why =>
      s"okay.rust.ApacheCData needs org.apache.arrow:arrow-c-data beside Arrow Java ($why) — " +
        "or use okay.rust.OkayCData, the default, which needs none of it")

  private def ready(): Unit = missing.foreach(why => throw IllegalStateException(why))

  def exporting[T](t: Table)(use: (MemorySegment, MemorySegment) => T): T =
    ready()
    val alloc = RootAllocator()
    try
      val root = ApacheArrow.toRoot(t, alloc)
      val a = ArrowArray.allocateNew(alloc)
      val s = ArrowSchema.allocateNew(alloc)
      try
        Data.exportVectorSchemaRoot(alloc, root, null, a, s)
        use(MemorySegment.ofAddress(s.memoryAddress()).reinterpret(CData.SchemaSize),
          MemorySegment.ofAddress(a.memoryAddress()).reinterpret(CData.ArraySize))
      finally
        // a consumer that did not release (it failed first) leaves it to us
        a.release(); a.close(); s.release(); s.close(); root.close()
    finally alloc.close()

  def importing(schema: MemorySegment, array: MemorySegment): Table =
    ready()
    val alloc = RootAllocator()
    try
      val root = Data.importVectorSchemaRoot(alloc, ArrowArray.wrap(array.address()), ArrowSchema.wrap(schema.address()), null)
      try ApacheArrow.fromRoot(root) finally root.close()
    finally alloc.close()
