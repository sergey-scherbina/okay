package okay.rust

import java.lang.foreign.{Arena, FunctionDescriptor, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle
import okay.Handler

/** the JVM's roads to the kernel (specs/polyglot-rust.md stages 1 and 3) */
trait KdfPlatform:
  /** `okay_argon2id`'s signature: two (pointer, length) inputs, three
   * parameters, an output buffer, an integer answer */
  private val signature = FunctionDescriptor.of(ValueLayout.JAVA_INT,
    ValueLayout.ADDRESS, ValueLayout.JAVA_LONG, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG,
    ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT,
    ValueLayout.ADDRESS, ValueLayout.JAVA_LONG)

  /** the Rust kernel (okay-rust/kernels/argon2), or why it cannot be bound */
  def rust(lib: NativeLib): Either[String, Handler[Kdf]] =
    lib.function("okay_argon2id", signature).map(mh => Kdf.using(call(mh, _)))

  /** the SAME kernel as WebAssembly, under Chicory: no native code in the
   * process (stage 3). The answers are the native road's, word for word */
  def wasm(lib: WasmLib): Handler[Kdf] =
    Kdf.using(op => lib.withBuffers { b =>
      val out = b.out(op.length)
      lib.call("okay_argon2id",
        b.in(op.password), op.password.length.toLong, b.in(op.salt), op.salt.length.toLong,
        op.memoryKb.toLong, op.iterations.toLong, op.parallelism.toLong, out, op.length.toLong)
        .flatMap { code =>
          if code == 0 then Right(b.read(out, op.length))
          else Left(s"okay_argon2id answered ${code.toInt}: ${Kdf.meaning(code.toInt)}")
        }
    })

  /** one call: every buffer allocated here, in a confined arena, and freed
   * when the call returns — nothing crosses the boundary to be freed later */
  private def call(mh: MethodHandle, op: Kdf.Argon2id): Either[String, Array[Byte]] =
    val arena = Arena.ofConfined()
    try
      def bytes(a: Array[Byte]): MemorySegment =
        val s = arena.allocate(math.max(1L, a.length.toLong))
        MemorySegment.copy(a, 0, s, ValueLayout.JAVA_BYTE, 0L, a.length)
        s
      val out = arena.allocate(math.max(1L, op.length.toLong))
      val answer = mh.invokeWithArguments(
        bytes(op.password), op.password.length.toLong, bytes(op.salt), op.salt.length.toLong,
        op.memoryKb, op.iterations, op.parallelism, out, op.length.toLong)
      answer match
        case code: java.lang.Integer if code.intValue == 0 => Right(out.asSlice(0L, op.length.toLong).toArray(ValueLayout.JAVA_BYTE))
        case code: java.lang.Integer => Left(s"okay_argon2id answered ${code.intValue}: ${Kdf.meaning(code.intValue)}")
        case other => Left(s"okay_argon2id answered $other, not an int")
    finally arena.close()
