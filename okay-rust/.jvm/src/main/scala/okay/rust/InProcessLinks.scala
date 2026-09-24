package okay.rust

import java.lang.foreign.{Arena, FunctionDescriptor, MemorySegment, ValueLayout}
import java.nio.charset.StandardCharsets.UTF_8
import okay.py.WireLink

/**
 * The okay wire IN THIS PROCESS (polyglot-one-wire, stage 3): a worker
 * library's `okay_exchange(req, len, out_len) -> resp` is the whole link —
 * one request line in, one answer line out, an empty request answering the
 * handshake. `ForeignWorker.over(link)` then drives a Rust or Go worker with
 * no second process at all, and `Foreign.program` / `Foreign.fn`,
 * multi-shot, direct style and `Durable` work exactly as over a pipe.
 */
object InProcessLinks:

  /** a Rust `cdylib` built with `okay::export_worker!`, through FFM */
  def ffm(lib: NativeLib): Either[String, WireLink] =
    for
      exchange <- lib.function("okay_exchange",
        FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG, ValueLayout.ADDRESS))
      free <- lib.function("okay_free", FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.JAVA_LONG))
    yield new WireLink:
      private def call(line: String): String =
        val arena = Arena.ofConfined()
        try
          val bytes = line.getBytes(UTF_8)
          val req = arena.allocate(math.max(1L, bytes.length.toLong))
          MemorySegment.copy(bytes, 0, req, ValueLayout.JAVA_BYTE, 0L, bytes.length)
          val outLen = arena.allocate(ValueLayout.JAVA_LONG)
          exchange.invokeWithArguments(req, bytes.length.toLong, outLen) match
            case resp: MemorySegment =>
              val n = outLen.get(ValueLayout.JAVA_LONG, 0L)
              // the answer is the library's memory, of the length it wrote:
              // read it, then hand it back to the allocator that made it
              val text = String(resp.reinterpret(n).toArray(ValueLayout.JAVA_BYTE), UTF_8)
              val _ = free.invokeWithArguments(resp, n)
              text
            case other => throw IllegalStateException(s"okay_exchange answered $other, not a pointer")
        finally arena.close()
      def hello(): Option[String] = Some(call(""))
      def roundTrip(line: String): Option[String] = Some(call(line))
      def close(): Unit = lib.close()

  /** a WebAssembly module exporting `okay_exchange` (Rust with
   * `okay::export_worker!`, or Go with `okay.Export`), under Chicory */
  def wasm(lib: WasmLib): WireLink = new WireLink:
    private def call(line: String): String =
      lib.withBuffers { b =>
        val bytes = line.getBytes(UTF_8)
        val req = b.in(bytes)
        val lenSlot = b.out(8)
        lib.call("okay_exchange", req, bytes.length.toLong, lenSlot) match
          case Left(why) => throw IllegalStateException(why)
          case Right(resp) =>
            // a wasm32 usize is 4 bytes, little-endian
            val n = java.nio.ByteBuffer.wrap(b.read(lenSlot, 4)).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt
            val text = String(b.read(resp, n), UTF_8)
            val _ = lib.call("okay_free", resp, n.toLong)
            text
      }
    def hello(): Option[String] = Some(call(""))
    def roundTrip(line: String): Option[String] = Some(call(line))
    def close(): Unit = ()
