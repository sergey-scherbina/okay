package okay.rust

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import okay.Handler

/**
 * The argon2 kernel's C function, as Scala Native sees it
 * (specs/polyglot-rust.md stage 2). The crate's `staticlib` is linked into
 * the binary by the build's linking options (scripts/rust-native-check.sh
 * names it), so this is an ordinary C call with no runtime between.
 */
@extern object Argon2Kernel:
  def okay_argon2id(password: Ptr[Byte], passwordLen: CSize, salt: Ptr[Byte], saltLen: CSize,
                    memoryKib: CUnsignedInt, iterations: CUnsignedInt, parallelism: CUnsignedInt,
                    out: Ptr[Byte], outLen: CSize): CInt = extern

/** Scala Native's road to the kernel */
trait PasswordHashPlatform:

  /** the staticlib, linked in: every buffer malloc'd here and freed after the call */
  def native: Handler[PasswordHash] = PasswordHash.using { op =>
    def copy(a: Array[Byte]): Ptr[Byte] =
      val p = stdlib.malloc(math.max(1, a.length).toCSize)
      var i = 0
      while i < a.length do { p(i) = a(i); i += 1 }
      p
    val pw = copy(op.password)
    val salt = copy(op.salt)
    val out = stdlib.malloc(math.max(1, op.length).toCSize)
    try
      val code = Argon2Kernel.okay_argon2id(pw, op.password.length.toCSize, salt, op.salt.length.toCSize,
        op.memoryKb.toUInt, op.iterations.toUInt, op.parallelism.toUInt, out, op.length.toCSize)
      if code == 0 then Right(Array.tabulate(op.length)(i => out(i)))
      else Left(s"okay_argon2id answered $code: ${PasswordHash.meaning(code)}")
    finally
      stdlib.free(pw)
      stdlib.free(salt)
      stdlib.free(out)
  }
