package okay.rust

import okay.{!, Handler}

/**
 * Hashing as an okay EFFECT (specs/polyglot-go.md, stage 2): a program asks
 * for SHA-256, and the handler decides what computes it — the JDK's
 * `MessageDigest` (`Digest.jdk`), or a Go plugin compiled to WebAssembly and
 * run by Chicory (`Digest.wasm`), with no Go runtime beside the JVM's.
 */
enum Digest[+A] derives okay.Effect:
  case Sha256(bytes: Array[Byte]) extends Digest[Either[String, Array[Byte]]]

object Digest:

  /** the SHA-256 of `bytes` */
  def sha256(bytes: Array[Byte]): Either[String, Array[Byte]] ! Digest =
    okay.effect[Digest, Either[String, Array[Byte]]](Sha256(bytes))

  /** the operations answered by `f` */
  def using(f: Sha256 => Either[String, Array[Byte]]): Handler[Digest] = new:
    def handle[A](e: Digest[A]): A = e match
      case op: Sha256 => f(op)

  /** the JDK's own SHA-256 */
  def jdk: Handler[Digest] =
    using(op => Right(java.security.MessageDigest.getInstance("SHA-256").digest(op.bytes)))

  /** a plugin exporting `okay_sha256(in, n, out)` (the Go plugin in
   * okay-rust/kernels/sha256-go), under Chicory */
  def wasm(lib: WasmLib): Handler[Digest] =
    using(op => lib.withBuffers { b =>
      val out = b.out(32)
      lib.call("okay_sha256", b.in(op.bytes), op.bytes.length.toLong, out).flatMap { code =>
        if code == 0 then Right(b.read(out, 32)) else Left(s"okay_sha256 answered $code")
      }
    })
