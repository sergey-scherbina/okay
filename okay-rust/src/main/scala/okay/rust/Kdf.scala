package okay.rust

import okay.{!, Handler}

/**
 * Key derivation as an okay EFFECT (specs/polyglot-rust.md, stage 1): a
 * program asks for Argon2id, and the handler decides what computes it —
 * the Rust kernel over FFM (`Kdf.rust`), or any function (`Kdf.using`: a
 * test's, or a JVM implementation). The program does not change.
 *
 * The handlers that reach a kernel are the PLATFORM's (`KdfPlatform`): on
 * the JVM `Kdf.rust` (FFM) and `Kdf.wasm` (Chicory); on Scala Native
 * `Kdf.native` (the staticlib through `@extern`).
 */
enum Kdf[+A] derives okay.Effect:
  case Argon2id(password: Array[Byte], salt: Array[Byte], memoryKb: Int, iterations: Int,
                parallelism: Int, length: Int) extends Kdf[Either[String, Array[Byte]]]

object Kdf extends KdfPlatform:

  /** Argon2id of `password` and `salt`, `length` bytes */
  def argon2id(password: Array[Byte], salt: Array[Byte], memoryKb: Int, iterations: Int,
               parallelism: Int, length: Int = 32): Either[String, Array[Byte]] ! Kdf =
    okay.effect[Kdf, Either[String, Array[Byte]]](Argon2id(password, salt, memoryKb, iterations, parallelism, length))

  /** the operations answered by `f` */
  def using(f: Argon2id => Either[String, Array[Byte]]): Handler[Kdf] = new:
    def handle[A](e: Kdf[A]): A = e match
      case op: Argon2id => f(op)

  private[rust] def meaning(code: Int): String = code match
    case -1 => "a null pointer where bytes are owed"
    case -2 => "parameters Argon2 refuses"
    case -3 => "hashing failed"
    case _ => "an answer the kernel does not document"
