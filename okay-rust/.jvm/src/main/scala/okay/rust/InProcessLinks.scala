package okay.rust

import java.lang.foreign.{Arena, FunctionDescriptor, MemorySegment, ValueLayout}
import java.nio.charset.StandardCharsets.UTF_8
import okay.foreign.WireLink

/**
 * The okay wire IN THIS PROCESS (polyglot-one-wire, stage 3): a worker
 * library's `okay_exchange(req, len, out_len) -> resp` is the whole link —
 * one request line in, one answer line out, an empty request answering the
 * handshake. `ForeignWorker.over(link)` then drives a Rust or Go worker with
 * no second process at all, and `Foreign.program` / `Foreign.fn`,
 * multi-shot, direct style and `Durable` work exactly as over a pipe.
 */
object InProcessLinks:

  /** a Rust `cdylib` built with `okay::export_worker!`, through FFM. A
   * library that also exports `okay_exchange_table` takes a table as the
   * Arrow C Data Interface, in place (foreign-arrow-ffm), through `codec` */
  def ffm(lib: NativeLib)(using codec: CDataCodec): Either[String, WireLink] =
    val P = ValueLayout.ADDRESS
    for
      exchangeFn <- lib.function("okay_exchange",
        FunctionDescriptor.of(ValueLayout.ADDRESS, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG, ValueLayout.ADDRESS))
      free <- lib.function("okay_free", FunctionDescriptor.ofVoid(ValueLayout.ADDRESS, ValueLayout.JAVA_LONG))
    yield new WireLink:
      private val tableFn = lib.function("okay_exchange_table",
        FunctionDescriptor.of(P, P, ValueLayout.JAVA_LONG, P, P, P, P, P)).toOption
      override def tables: Option[WireLink.Tables] = tableFn.map { fn =>
        new WireLink.Tables:
          def exchange(message: Array[Byte], table: okay.arrow.Table): (Array[Byte], Option[okay.arrow.Table]) =
            codec.exporting(table) { (schemaIn, arrayIn) =>
              val arena = Arena.ofConfined()
              try
                val req = arena.allocate(math.max(1L, message.length.toLong))
                MemorySegment.copy(message, 0, req, ValueLayout.JAVA_BYTE, 0L, message.length)
                val outLen = arena.allocate(ValueLayout.JAVA_LONG)
                // zeroed: a release still NULL after the call is "no table came back"
                val schemaOut = arena.allocate(CData.SchemaSize)
                val arrayOut = arena.allocate(CData.ArraySize)
                fn.invokeWithArguments(req, message.length.toLong, outLen, schemaIn, arrayIn, schemaOut, arrayOut) match
                  case resp: MemorySegment =>
                    val n = outLen.get(ValueLayout.JAVA_LONG, 0L)
                    val answer = resp.reinterpret(n).toArray(ValueLayout.JAVA_BYTE)
                    val _ = free.invokeWithArguments(resp, n)
                    val got = Option.when(CData.ptr(arrayOut, CData.ARelease).address != 0L)(codec.importing(schemaOut, arrayOut))
                    (answer, got)
                  case other => throw IllegalStateException(s"okay_exchange_table answered $other, not a pointer")
              finally arena.close()
            }
      }
      private def call(line: String): String = String(callBytes(line.getBytes(UTF_8)), UTF_8)
      private def callBytes(bytes: Array[Byte]): Array[Byte] =
        val arena = Arena.ofConfined()
        try
          val req = arena.allocate(math.max(1L, bytes.length.toLong))
          MemorySegment.copy(bytes, 0, req, ValueLayout.JAVA_BYTE, 0L, bytes.length)
          val outLen = arena.allocate(ValueLayout.JAVA_LONG)
          exchangeFn.invokeWithArguments(req, bytes.length.toLong, outLen) match
            case resp: MemorySegment =>
              val n = outLen.get(ValueLayout.JAVA_LONG, 0L)
              // the answer is the library's memory, of the length it wrote:
              // read it, then hand it back to the allocator that made it
              val answer = resp.reinterpret(n).toArray(ValueLayout.JAVA_BYTE)
              val _ = free.invokeWithArguments(resp, n)
              answer
            case other => throw IllegalStateException(s"okay_exchange answered $other, not a pointer")
        finally arena.close()
      def hello(): Option[String] = Some(call(""))
      def roundTrip(line: String): Option[String] = Some(call(line))
      def exchange(message: Array[Byte]): Option[Array[Byte]] = Some(callBytes(message))
      def close(): Unit = lib.close()
      override def inProcess: Boolean = true

  /** a WebAssembly module exporting `okay_exchange` (Rust with
   * `okay::export_worker!`, or Go with `okay.Export`), under Chicory */
  def wasm(lib: WasmLib): WireLink = new WireLink:
    private def call(line: String): String = String(callBytes(line.getBytes(UTF_8)), UTF_8)
    private def callBytes(bytes: Array[Byte]): Array[Byte] =
      lib.withBuffers { b =>
        val req = b.in(bytes)
        val lenSlot = b.out(8)
        lib.call("okay_exchange", req, bytes.length.toLong, lenSlot) match
          case Left(why) => throw IllegalStateException(why)
          case Right(resp) =>
            // a wasm32 usize is 4 bytes, little-endian
            val n = java.nio.ByteBuffer.wrap(b.read(lenSlot, 4)).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt
            val answer = b.read(resp, n)
            val _ = lib.call("okay_free", resp, n.toLong)
            answer
      }
    def hello(): Option[String] = Some(call(""))
    def roundTrip(line: String): Option[String] = Some(call(line))
    def exchange(message: Array[Byte]): Option[Array[Byte]] = Some(callBytes(message))
    def close(): Unit = ()
    override def inProcess: Boolean = true
