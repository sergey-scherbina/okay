package okay.rust

import java.lang.foreign.{Arena, FunctionDescriptor, MemorySegment, ValueLayout}
import java.lang.invoke.MethodHandle
import okay.{!, Handler}

/**
 * Key derivation as an okay EFFECT (specs/polyglot-rust.md, stage 1): a
 * program asks for Argon2id, and the handler decides what computes it —
 * the Rust kernel over FFM (`Kdf.rust`), or any function (`Kdf.using`: a
 * test's, or a JVM implementation). The program does not change.
 */
enum Kdf[+A] derives okay.Effect:
  case Argon2id(password: Array[Byte], salt: Array[Byte], memoryKb: Int, iterations: Int,
                parallelism: Int, length: Int) extends Kdf[Either[String, Array[Byte]]]

object Kdf:

  /** Argon2id of `password` and `salt`, `length` bytes */
  def argon2id(password: Array[Byte], salt: Array[Byte], memoryKb: Int, iterations: Int,
               parallelism: Int, length: Int = 32): Either[String, Array[Byte]] ! Kdf =
    okay.effect[Kdf, Either[String, Array[Byte]]](Argon2id(password, salt, memoryKb, iterations, parallelism, length))

  /** the operations answered by `f` */
  def using(f: Argon2id => Either[String, Array[Byte]]): Handler[Kdf] = new:
    def handle[A](e: Kdf[A]): A = e match
      case op: Argon2id => f(op)

  /** `okay_argon2id`'s signature: two (pointer, length) inputs, three
   * parameters, an output buffer, an integer answer */
  private val signature = FunctionDescriptor.of(ValueLayout.JAVA_INT,
    ValueLayout.ADDRESS, ValueLayout.JAVA_LONG, ValueLayout.ADDRESS, ValueLayout.JAVA_LONG,
    ValueLayout.JAVA_INT, ValueLayout.JAVA_INT, ValueLayout.JAVA_INT,
    ValueLayout.ADDRESS, ValueLayout.JAVA_LONG)

  /** the Rust kernel (okay-rust/kernels/argon2), or why it cannot be bound */
  def rust(lib: NativeLib): Either[String, Handler[Kdf]] =
    lib.function("okay_argon2id", signature).map(mh => using(call(mh, _)))

  /** the SAME kernel as WebAssembly, under Chicory: no native code in the
   * process (stage 3). The answers are the native road's, word for word */
  def wasm(lib: WasmLib): Handler[Kdf] =
    using(op => lib.withBuffers { b =>
      val out = b.out(op.length)
      lib.call("okay_argon2id",
        b.in(op.password), op.password.length.toLong, b.in(op.salt), op.salt.length.toLong,
        op.memoryKb.toLong, op.iterations.toLong, op.parallelism.toLong, out, op.length.toLong)
        .flatMap { code =>
          if code == 0 then Right(b.read(out, op.length))
          else Left(s"okay_argon2id answered ${code.toInt}: ${meaning(code.toInt)}")
        }
    })

  private def meaning(code: Int): String = code match
    case -1 => "a null pointer where bytes are owed"
    case -2 => "parameters Argon2 refuses"
    case -3 => "hashing failed"
    case _ => "an answer the kernel does not document"

  /** one call: every buffer allocated here, in a confined arena, and freed
   * when the call returns — nothing crosses the boundary to be freed later */
  private def call(mh: MethodHandle, op: Argon2id): Either[String, Array[Byte]] =
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
        case code: java.lang.Integer => Left(s"okay_argon2id answered ${code.intValue}: ${meaning(code.intValue)}")
        case other => Left(s"okay_argon2id answered $other, not an int")
    finally arena.close()
