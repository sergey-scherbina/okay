package okay.rust

import okay.{Handler, given}

/** one Argon2id case and its bytes, as BouncyCastle (the JVM) computes them */
final case class Golden(password: String, salt: String, m: Int, t: Int, p: Int, n: Int, hex: String)

/**
 * The same bytes on every platform (specs/polyglot-rust.md stage 2). The
 * vectors are PINNED here, shared by the JVM and Scala Native: on the JVM the
 * suite runs under BouncyCastle, which keeps the pins honest, and on Native
 * under the Rust staticlib, which is then held to the same bytes.
 */
abstract class KdfGoldenSuite extends munit.FunSuite:

  /** what computes Argon2id on this platform */
  def handler: Handler[Kdf]

  val golden: Vector[Golden] = Vector(
    Golden("", "saltsalt", 8, 1, 1, 16, "b7ffc76d23b515687c3164bb8386cbe9"),
    Golden("correct horse battery staple", "saltsalt", 64, 2, 1, 32, "174116055b8170e281d9d0531d949dbbc9269ab738ae8140d5f3232400363b7e"),
    Golden("pw", "a longer salt, sixteen+", 256, 3, 2, 32, "722e8c32dcd0ec6672a4d56a4185081cf821cde62d4530c4ca39b607de3e5686"),
    Golden("correct horse battery staple", "a longer salt, sixteen+", 1024, 1, 4, 64, "c2b7644a0043cb49073469b4993776cc1089cd1823894b99ddfb5e507a798155b9ecb98ed105ab1b3e7300e6b035b42f3b3f9814906efaa1e8a91f4dfd8f4f7a"),
  )

  private def hex(bs: Array[Byte]): String = bs.map(b => f"${b & 0xff}%02x").mkString

  test("the pinned vectors, under this platform's handler") {
    val got = golden.map { g =>
      Kdf.argon2id(g.password.getBytes("UTF-8"), g.salt.getBytes("UTF-8"), g.m, g.t, g.p, g.n)
        .runWith(using handler).map(hex)
    }
    assertEquals(got, golden.map(g => Right(g.hex)))
  }

  test("a refused parameter set is a Left naming the kernel's answer") {
    val r = Kdf.argon2id("pw".getBytes("UTF-8"), "saltsalt".getBytes("UTF-8"), 1, 1, 1).runWith(using handler)
    assert(r.isLeft, s"$r")
  }
