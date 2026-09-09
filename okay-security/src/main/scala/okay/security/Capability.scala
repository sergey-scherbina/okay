package okay.security

import okay.Uid

/**
 * A CAPABILITY THAT ITS HOLDER CAN NARROW (specs/coordination-free.md
 * stage 4), macaroon-shaped.
 *
 * WHAT THE REST OF THIS MODULE CANNOT DO. `Jwt`, `Es256`, `OAuth2`,
 * `Oidc` all have the same shape: an issuer signs, a verifier checks,
 * and everything in between is the issuer's word. A holder who wants
 * to hand a colleague *less* than it has must go back to the issuer
 * and ask for a smaller token, because narrowing a JWT breaks its
 * signature exactly as forging one does. The signature cannot tell
 * the two apart, and that is the gap this fills.
 *
 * THE TRICK, and it is one line of the file: each caveat is signed
 * with the PREVIOUS SIGNATURE as the key.
 *
 * {{{
 * tag0 = HMAC(rootKey, id || subject)
 * tag1 = HMAC(tag0,    caveat1)
 * tag2 = HMAC(tag1,    caveat2)
 * }}}
 *
 * Adding a caveat needs only `tag2` — which the holder has, because
 * it IS the token. Removing one would need `tag1` to re-derive from,
 * and `tag1` is gone: it was consumed to make `tag2` and never
 * travels. So **anyone can narrow and nobody can widen**, without a
 * key, without the issuer, and without anything to look up. A
 * verifier holding the root key recomputes the chain from scratch and
 * compares.
 *
 * A CAVEAT IS OPAQUE TEXT, deliberately. The library signs it and
 * refuses to interpret it; the relying party decides what it means,
 * because only the relying party knows what its own requests look
 * like. `Caveat.until` and `Caveat.scope` are conveniences over that,
 * not a schema — a caveat this library has never heard of still
 * chains, still travels and still has to be satisfied.
 *
 * TWO THINGS THIS IS NOT.
 *
 * It is not encryption: a capability is readable by anyone holding
 * it, and its caveats are public. Put no secret in a caveat.
 *
 * It is not revocation. A capability is valid until it expires, and
 * nothing here can reach out and cancel one — that is the price of a
 * verifier that needs no registry. What the design DOES give is a
 * cheap revocation *check* when you want one: `id` is a `Uid`, which
 * is sortable by issue time, so "everything issued before T is void"
 * is a comparison rather than a list.
 *
 * (An earlier draft of the spec said a capability's `until` wants an
 * `Hlc`. That was wrong and is corrected there: an expiry is checked
 * by the VERIFIER against the verifier's own clock, and a logical
 * clock orders events without making a deadline trustworthy. The
 * reuse that is real is `Uid`, above.)
 */
final case class Capability(
    id: String,
    subject: String,
    caveats: Vector[String],
    tag: String,
):

  /**
   * Narrow this capability. NO KEY IS NEEDED and that is the whole
   * point — a holder attenuates what it holds and passes it on, with
   * the issuer nowhere in sight.
   */
  def attenuate(caveat: String)(using c: Crypto): Capability =
    // no restriction on the text: each caveat is HMAC'd on its OWN,
    // never concatenated with another, so there is no separator for
    // it to smuggle
    val next = c.hmacSha256(Capability.unhex(tag), Capability.bytes(caveat))
    Capability(id, subject, caveats :+ caveat, Capability.hex(next))

  def attenuate(caveat: Caveat)(using Crypto): Capability = attenuate(caveat.text)

  /**
   * Check the chain against the root key AND every caveat against the
   * request. Both halves are required: a perfect signature over an
   * expired token is a valid token that must be refused, so a caller
   * that only wants the signature is asking the wrong question and
   * this method does not offer it.
   *
   * `satisfied` decides one caveat at a time. Anything it does not
   * recognise should be REFUSED rather than ignored — an unknown
   * caveat is a restriction this verifier cannot enforce, and
   * ignoring it grants more than the token says. `Capability.checking`
   * builds a verifier with that default.
   */
  def verify(rootKey: Array[Byte], satisfied: String => Boolean)(using c: Crypto): Boolean =
    val expected = Capability.chain(rootKey, id, subject, caveats)
    Crypto.constantTimeEquals(Capability.unhex(tag), expected) &&
      caveats.forall(satisfied)

  /** the wire form: `okc1.<id>.<subject>.<caveat>*.<tag>`, each part
   * base64url without padding, so it survives a header, a URL and a
   * JSON string with nothing escaped */
  def encoded: String =
    val parts = Vector(Capability.Version, Capability.b64(id), Capability.b64(subject)) ++
      caveats.map(Capability.b64) :+ tag
    parts.mkString(".")

object Capability:

  private final val Version = "okc1"

  /**
   * Issue a root capability. The id is a `Uid` — unique with no
   * coordination, and sortable by issue time, which is what makes a
   * "nothing issued before T" rule a comparison.
   */
  def issue(rootKey: Array[Byte], subject: String)(using c: Crypto): Capability =
    val id = Uid.next().ulid
    Capability(id, subject, Vector.empty, hex(chain(rootKey, id, subject, Vector.empty)))

  /** the same, with the id supplied — for a caller that mints ids its
   * own way, and for a test that wants a fixed one */
  def issueAs(rootKey: Array[Byte], id: String, subject: String)(using c: Crypto): Capability =
    Capability(id, subject, Vector.empty, hex(chain(rootKey, id, subject, Vector.empty)))

  /**
   * The id, then the subject, then one HMAC per caveat, each keyed
   * by the running tag.
   *
   * The id and subject are CHAINED rather than concatenated, and
   * that is not fussiness. Signing `id + sep + subject` needs a
   * separator no field can contain, and every such scheme is one
   * surprising input away from two different pairs signing the same
   * bytes. Chaining leaves no separator to smuggle -- and the first
   * draft of this file put a literal NUL in the source to be that
   * separator, which is exactly the kind of cleverness this avoids.
   */
  private def chain(rootKey: Array[Byte], id: String, subject: String,
                    caveats: Vector[String])(using c: Crypto): Array[Byte] =
    var tag = c.hmacSha256(c.hmacSha256(rootKey, bytes(id)), bytes(subject))
    var i = 0
    while i < caveats.length do
      tag = c.hmacSha256(tag, bytes(caveats(i)))
      i += 1
    tag

  /** parse the wire form; anything malformed answers None rather than
   * throwing, because this string arrives from outside */
  def decode(s: String): Option[Capability] =
    val parts = s.split('.')
    if parts.length < 4 || parts(0) != Version then None
    else
      val id = unb64(parts(1))
      val subject = unb64(parts(2))
      val caveats = parts.slice(3, parts.length - 1).toVector.map(unb64)
      val tag = parts(parts.length - 1)
      if id.isEmpty || subject.isEmpty || caveats.exists(_.isEmpty) || !isHex(tag) then None
      else Some(Capability(id.get, subject.get, caveats.map(_.get), tag))

  /**
   * A verifier over a set of known caveat kinds, refusing anything it
   * does not recognise. That default is the safe one and it is not a
   * matter of taste: an unrecognised caveat is a restriction this
   * verifier cannot enforce, and ignoring it grants MORE than the
   * token says.
   */
  def checking(now: Long, scopes: Set[String]): String => Boolean = text =>
    Caveat.parse(text) match
      case Some(Caveat.Until(millis)) => now < millis
      case Some(Caveat.Scope(name)) => scopes.contains(name)
      case None => false        // unknown: refuse, never ignore

  // ── bytes and spellings ────────────────────────────────────────

  private def bytes(s: String): Array[Byte] =
    s.getBytes(java.nio.charset.StandardCharsets.UTF_8)

  private final val HexDigits = "0123456789abcdef"

  private def hex(b: Array[Byte]): String =
    val sb = new StringBuilder(b.length * 2)
    var i = 0
    while i < b.length do
      val v = b(i) & 0xFF
      // `val _ =` because append ANSWERS the builder, and a discarded
      // answer is a warning in this build
      val _ = sb.append(HexDigits.charAt(v >>> 4)).append(HexDigits.charAt(v & 0xF))
      i += 1
    sb.toString

  private def isHex(s: String): Boolean =
    s.length % 2 == 0 && s.length > 0 && {
      var i = 0
      var ok = true
      while i < s.length && ok do
        if Character.digit(s.charAt(i), 16) < 0 then ok = false else i += 1
      ok
    }

  private def unhex(s: String): Array[Byte] =
    val out = new Array[Byte](s.length / 2)
    var i = 0
    while i < out.length do
      out(i) = ((Character.digit(s.charAt(2 * i), 16) << 4) |
        Character.digit(s.charAt(2 * i + 1), 16)).toByte
      i += 1
    out

  /** base64url WITHOUT padding: `=` is not URL-safe in every context
   * and the length tells the decoder what it needs */
  private def b64(s: String): String =
    java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(bytes(s))

  private def unb64(s: String): Option[String] =
    try Some(new String(java.util.Base64.getUrlDecoder.decode(s),
      java.nio.charset.StandardCharsets.UTF_8))
    catch case _: IllegalArgumentException => None

/**
 * The caveats this library knows how to spell. It signs any text at
 * all — see `Capability.attenuate(String)` — and these exist so the
 * common two are written the same way by everyone who uses them.
 */
enum Caveat(val text: String):
  /** valid strictly before this Unix millisecond, by the VERIFIER's
   * clock: nothing in a token can make a deadline trustworthy */
  case Until(millis: Long) extends Caveat(s"until=$millis")
  /** one named permission; several scopes are several caveats, and
   * they intersect, because every caveat must hold */
  case Scope(name: String) extends Caveat(s"scope=$name")

object Caveat:
  def parse(text: String): Option[Caveat] =
    if text.startsWith("until=") then
      text.substring(6).toLongOption.map(Caveat.Until.apply)
    else if text.startsWith("scope=") then
      val n = text.substring(6)
      if n.isEmpty then None else Some(Caveat.Scope(n))
    else None
