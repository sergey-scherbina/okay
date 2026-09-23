package okay.scala2

import okay.codec.Schema

/**
 * JSON for Scala 2.13 (specs/scala2-facade.md, stage 6).
 *
 * okay-codec's `Json` is the one part of the codec a Scala 2 compiler
 * cannot read: its TASTy makes scalac 2.13.18's reader throw
 * ("class file ... is broken (class scala.MatchError/49)"), and with it
 * every signature that names the `Json` value type. So this object
 * speaks JSON as TEXT, and the `Json` type appears in none of its
 * signatures. Underneath it is okay-codec's own encoder and decoder,
 * unchanged.
 */
object Json {

  def write[A](a: A)(using s: Schema[A]): String = okay.codec.Json.write(a)

  /** a decode error is a `Left` naming the problem; an absent OPTIONAL
   * field reads as `None` */
  def read[A](text: String)(using s: Schema[A]): Either[String, A] = okay.codec.Json.read[A](text)

  /** the same, but refusing a document that okay-codec's tolerant
   * reader would repair (damage, trailing garbage) */
  def readStrict[A](text: String)(using s: Schema[A]): Either[String, A] = okay.codec.Json.readStrict[A](text)
}

/** the JSON Schema that declares `A`, as text */
object JsonSchema {
  def of[A](s: Schema[A]): String = okay.codec.Json.print(okay.codec.JsonSchema.of(s))
}
