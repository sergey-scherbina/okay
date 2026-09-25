package okay2.codec

/** the codecs' shared constants (okay-codec's Codecs.scala; its
 * provider registry is not ported — nothing here has a second one) */
object Codecs {

  /** how many open containers a recursive walk takes on the native
   * stack before it continues on a `Cont.defer` trampoline: fast for
   * every ordinary document, bounded by the heap for a deep one */
  val NativeThreshold: Int = 24
}
