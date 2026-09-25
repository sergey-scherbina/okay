package okay.arrow.bench

import okay.arrow.ArrowJava
import okay.codec.ArrowIpc
import org.apache.arrow.memory.{BufferAllocator, RootAllocator}
import org.apache.arrow.vector.VectorSchemaRoot
import org.openjdk.jmh.annotations.*

/**
 * STAGE 0 of specs/okay-arrow.md: okay's `ArrowIpc` against Arrow Java
 * 19.0.0 on one table (float64, int64, utf8 with a null in every tenth
 * row), the same IPC bytes on both sides (TestArrowJavaInterop).
 *
 * Four pairs, each naming what it includes (docs/benchmarks.md lane
 * rules):
 *   write_arrays  JVM arrays -> IPC bytes (Arrow: fill vectors, write, free)
 *   write_native  each side's own columns -> IPC bytes (ours ARE arrays;
 *                 Arrow's root is filled once in setup: its best case)
 *   read_arrays   IPC bytes -> JVM arrays (Arrow: load, copy out, free)
 *   read_native   IPC bytes -> each side's own columns (ours are arrays;
 *                 Arrow: load the batch and touch the row count, free —
 *                 its best case, no copy out)
 * Both sides read Arrow Java's bytes. Arrow runs with its unsafe
 * allocator, the one its docs recommend for speed.
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(java.util.concurrent.TimeUnit.MILLISECONDS)
@Warmup(iterations = 5, time = 2)
@Measurement(iterations = 5, time = 2)
@Fork(value = 1, jvmArgsAppend = Array(
  "--add-opens=java.base/java.nio=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow",
  "--enable-native-access=ALL-UNNAMED"))
class ArrowIpcBench:

  @Param(Array("500000"))
  var rows: Int = 500000

  private var d: ArrowJava.Data = null
  private var alloc: BufferAllocator = null
  private var root: VectorSchemaRoot = null
  private var bytes: Array[Byte] = null

  @Setup def setup(): Unit =
    d = ArrowJava.data(rows)
    alloc = RootAllocator()
    root = ArrowJava.fill(alloc, d)
    bytes = ArrowJava.write(root)
    val ours = ArrowIpc.write(d.okay)
    if !ArrowJava.same(d, ArrowJava.okayToArrays(bytes)) || !ArrowJava.same(d, ArrowJava.readToArrays(alloc, ours)) then
      throw IllegalStateException("the two sides do not read each other's bytes: no number")
    println(s"\nIPC-BYTES rows=$rows arrow-java=${bytes.length} okay=${ours.length}")

  @TearDown def tearDown(): Unit =
    root.close(); alloc.close()

  @Benchmark def write_arrays_okay(): Array[Byte] = ArrowIpc.write(d.okay)
  @Benchmark def write_arrays_arrow(): Array[Byte] = ArrowJava.writeFromArrays(alloc, d)

  @Benchmark def write_native_okay(): Array[Byte] = ArrowIpc.write(d.okay)
  @Benchmark def write_native_arrow(): Array[Byte] = ArrowJava.write(root)

  @Benchmark def read_arrays_okay(): ArrowJava.Data = ArrowJava.okayToArrays(bytes)
  @Benchmark def read_arrays_arrow(): ArrowJava.Data = ArrowJava.readToArrays(alloc, bytes)

  @Benchmark def read_native_okay(): ArrowIpc.Table = ArrowIpc.read(bytes)
  @Benchmark def read_native_arrow(): Int = ArrowJava.read(alloc, bytes)(_.getRowCount)
