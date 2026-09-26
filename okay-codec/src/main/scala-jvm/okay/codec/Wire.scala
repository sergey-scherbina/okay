package okay.codec

import java.io.{BufferedInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * How a wire message's tree becomes bytes (polyglot-one-wire stage 5), chosen
 * at COMPILE TIME by the given in scope where an engine is opened — the
 * foreign workers' (`okay.foreign.ForeignWorker`) and R's (`okay.r.RSubprocess`)
 * alike:
 *
 * {{{
 * import okay.codec.WireFormat.Cbor.given        // CBOR instead of JSON
 * import okay.codec.WireCompression.Off.given    // no compression at all
 * }}}
 *
 * With no import the format is JSON and compression is PREFERRED: deflate,
 * else zlib, else none, whichever the far side announces first, and none on
 * an in-process link. An EXPLICIT choice (`WireFormat.Cbor.given`,
 * `WireCompression.Deflate.given`, `WireCompression.Zlib.given`) that the far
 * side did not announce is refused by name, never quietly downgraded.
 */
trait WireFormat:
  def name: String
  def encode(tree: Json): Array[Byte]
  def decode(bytes: Array[Byte]): Json

object WireFormat:
  /** JSON, the default */
  given json: WireFormat = new WireFormat:
    def name = "json"
    def encode(tree: Json): Array[Byte] = Json.print(tree).getBytes(UTF_8)
    def decode(bytes: Array[Byte]): Json = WireJson.whole(String(bytes, UTF_8))

  /** `import okay.codec.WireFormat.Cbor.given` */
  object Cbor:
    given cbor: WireFormat = new WireFormat:
      def name = "cbor"
      def encode(tree: Json): Array[Byte] = WireCbor.encode(tree)
      def decode(bytes: Array[Byte]): Json = WireCbor.decode(bytes).fold(why => throw IllegalStateException(why), identity)

  /** the given above, chosen at RUNTIME from a name instead of an import —
   * a config value or a flag, for `WireChoice.named` */
  def byName(name: String): Either[String, WireFormat] = name match
    case "json" => Right(json)
    case "cbor" => Right(Cbor.cbor)
    case other => Left(s"unknown wire format '$other' (json, cbor)")

trait WireCompression:
  def name: String
  def compress(bytes: Array[Byte]): Array[Byte]
  def decompress(bytes: Array[Byte]): Array[Byte]
  /** what to try instead where this one is not to be had — a far side that
   * did not announce it, or a link that is not a network one, where it
   * would only cost (wire-compression-measured).
   * None (every explicit choice): refuse by name instead. */
  def fallback: Option[WireCompression] = None

object WireCompression:
  /** THE DEFAULT (operator, 2026-09-24): an ORDER of preference — raw
   * deflate, then zlib (what R can check natively), then the plain wire —
   * on a NETWORK link only. On a pipe and in-process the plain wire:
   * measured (WireCodecBench, wire-compression-measured), DEFLATE made a
   * short message LONGER (51 -> 53 bytes) and every round trip slower
   * (+2.3 us small, +9 us medium, +0.9 ms large, pooled), which a pipe's
   * bandwidth never pays back; over a network the medium message's
   * 1620 -> 355 bytes does. An explicit `Deflate`/`Zlib` still compresses
   * anywhere. */
  given preferred: WireCompression = new Zipped(nowrap = true):
    override def fallback: Option[WireCompression] = Some(new Zipped(nowrap = false):
      override def fallback: Option[WireCompression] = Some(Off.off))

  /** `import okay.codec.WireCompression.Off.given`: never compress */
  object Off:
    given off: WireCompression = new WireCompression:
      def name = "none"
      def compress(bytes: Array[Byte]): Array[Byte] = bytes
      def decompress(bytes: Array[Byte]): Array[Byte] = bytes

  /** `import okay.codec.WireCompression.Deflate.given`: raw DEFLATE (RFC 1951)
   * REQUIRED — a far side without it is refused by name, and in-process
   * links compress too */
  object Deflate:
    given deflate: WireCompression = new Zipped(nowrap = true)

  /** `import okay.codec.WireCompression.Zlib.given`: zlib (RFC 1950: DEFLATE
   * with a header and an adler32 check) REQUIRED */
  object Zlib:
    given zlib: WireCompression = new Zipped(nowrap = false)

  /** the givens above, chosen at RUNTIME from a name instead of an import —
   * "auto" is `preferred`; a config value or a flag, for `WireChoice.named` */
  def byName(name: String): Either[String, WireCompression] = name match
    case "auto" => Right(preferred)
    case "none" => Right(Off.off)
    case "deflate" => Right(Deflate.deflate)
    case "zlib" => Right(Zlib.zlib)
    case other => Left(s"unknown wire compression '$other' (auto, none, deflate, zlib)")

  /** DEFLATE from java.util.zip: raw (`nowrap`), which every far side's
   * standard library has (zlib's `wbits=-15`, Go's compress/flate, Node's
   * inflateRaw, Rust's flate2), or zlib-wrapped, which R's memCompress is */
  private class Zipped(nowrap: Boolean) extends WireCompression:
    def name = if nowrap then "deflate" else "zlib"
    // a Deflater holds ~256 KB of native zlib state, and a new one per
    // message with 8 KB buffers cost a short message 18.9 KB of garbage per
    // round trip; kept and reset, 2.4 KB, and 4.1 -> 3.1 us
    // (WireCodecBench, wire-compression-measured). A pool rather than a
    // ThreadLocal, because a virtual thread per call would pin one each.
    private val deflaters = java.util.concurrent.ConcurrentLinkedQueue[java.util.zip.Deflater]()
    private val inflaters = java.util.concurrent.ConcurrentLinkedQueue[java.util.zip.Inflater]()

    def compress(bytes: Array[Byte]): Array[Byte] =
      val d = Option(deflaters.poll()).getOrElse(
        java.util.zip.Deflater(java.util.zip.Deflater.DEFAULT_COMPRESSION, nowrap))
      var whole = false
      try
        d.setInput(bytes)
        d.finish()
        // DEFLATE's worst case is the input plus 5 bytes per 16 KB stored block
        var out = new Array[Byte](bytes.length + bytes.length / 16000 * 5 + 64)
        var n = 0
        while !d.finished() do
          if n == out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
          n += d.deflate(out, n, out.length - n)
        whole = true
        java.util.Arrays.copyOf(out, n)
      finally
        if whole && deflaters.size < Zipped.Kept then { d.reset(); val _ = deflaters.offer(d) }
        else d.end()

    def decompress(bytes: Array[Byte]): Array[Byte] =
      val i = Option(inflaters.poll()).getOrElse(java.util.zip.Inflater(nowrap))
      var whole = false
      try
        i.setInput(bytes)
        var out = new Array[Byte](math.max(64, bytes.length * 4))
        var n = 0
        while !i.finished() do
          if n == out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
          val got = i.inflate(out, n, out.length - n)
          // an empty message finishes on a step that yields nothing
          if got == 0 && !i.finished() && (i.needsInput() || i.needsDictionary()) then
            throw IllegalStateException("a DEFLATE message ended before its data did (cut short?)")
          n += got
        whole = true
        java.util.Arrays.copyOf(out, n)
      finally
        // one that refused a message is not trusted with the next
        if whole && inflaters.size < Zipped.Kept then { i.reset(); val _ = inflaters.offer(i) }
        else i.end()

  private object Zipped:
    /** per direction, per compression given: a far side is one exchange at a time */
    val Kept = 4

/**
 * How a FRAME crosses (py-arrow, specs/py-arrow.md): as the wire's JSON
 * frame, or as an Arrow IPC stream where the far side announces
 * `"frames":["arrow"]` (a Python worker with pyarrow).
 *
 * {{{
 * // no import: Arrow where spoken, else JSON, never refused
 * import okay.codec.FrameFormat.Json.given     // never Arrow
 * import okay.codec.FrameFormat.Arrow.given    // Arrow REQUIRED
 * }}}
 */
final case class FrameFormat(name: String, strict: Boolean)

object FrameFormat:
  /** THE DEFAULT: measured, the Python side of a 500k-row frame is 909 ms
   * on the JSON road and 0.5 ms as Arrow; a preference, not a demand */
  given preferred: FrameFormat = FrameFormat("arrow", strict = false)
  object Json:
    given json: FrameFormat = FrameFormat("json", strict = true)
  object Arrow:
    given arrow: FrameFormat = FrameFormat("arrow", strict = true)

  /** the givens above, chosen at RUNTIME from a name instead of an import —
   * "auto" is `preferred`; a config value or a flag, for `WireChoice.named` */
  def byName(name: String): Either[String, FrameFormat] = name match
    case "auto" => Right(preferred)
    case "json" => Right(Json.json)
    case "arrow" => Right(Arrow.arrow)
    case other => Left(s"unknown frame format '$other' (auto, json, arrow)")

/**
 * `WireFormat`, `WireCompression` and `FrameFormat` as one EXPLICIT value,
 * for a caller that picks the wire at RUNTIME — from a flag or a config
 * file, not a compile-time `given` import (the three types above stay
 * given-based; this is an alternative entry point beside them, not a
 * replacement): `ForeignWorker.startWithWire`, `RSubprocess.startWithWire`.
 *
 * {{{
 * WireChoice.named(format = "cbor", frames = "json")   // Left on a bad name
 * }}}
 */
final case class WireChoice(format: WireFormat = WireFormat.json,
                             compression: WireCompression = WireCompression.preferred,
                             frames: FrameFormat = FrameFormat.preferred,
                             deadline: WireDeadline = WireDeadline.none)

object WireChoice:
  /** the same wire the given-based defaults pick with no import at all */
  val default: WireChoice = WireChoice()

  /** `format`: "json" or "cbor"; `compression`: "auto", "none", "deflate" or
   * "zlib"; `frames`: "auto", "json" or "arrow" (arrow REQUIRED: refused if
   * the far side does not speak it) — a name outside these is a `Left`
   * naming it and its choices, never a silent fallback */
  def named(format: String = "json", compression: String = "auto", frames: String = "auto"): Either[String, WireChoice] =
    for
      f <- WireFormat.byName(format)
      c <- WireCompression.byName(compression)
      fr <- FrameFormat.byName(frames)
    yield WireChoice(f, c, fr)

/**
 * Who may speak on the wire (polyglot-one-wire stage 5b, wire-auth): a
 * MUTUAL HMAC-SHA256 challenge at the handshake. Each side proves it holds
 * the secret without sending it. The secret comes from the source the given
 * names, read when a worker is opened:
 *
 * {{{
 * given WireAuth = WireAuth.fromEnv("OKAY_WIRE_SECRET")
 * given WireAuth = WireAuth.fromFile(Path.of("/run/secrets/okay"))
 * }}}
 *
 * No given means no authentication, as before. It does not encrypt, and a
 * relay in the middle can pass the handshake through: TLS is the layer for
 * that, and the two compose.
 */
sealed trait WireAuth:
  /** where the secret comes from, for a refusal to name */
  def source: String

object WireAuth:
  /** the default: no authentication */
  given none: WireAuth = Off

  case object Off extends WireAuth:
    def source = "no given WireAuth"

  /** a secret, read when a worker is opened; a source that has none fails
   * THEN, by name, rather than authenticating with an empty key */
  final class Hmac private[WireAuth] (val source: String, read: () => Array[Byte]) extends WireAuth:
    def key(): Array[Byte] =
      val k = read()
      if k.isEmpty then throw IllegalStateException(s"the wire secret from $source is empty")
      k

  def secret(bytes: Array[Byte]): WireAuth = Hmac("a secret in the program", () => bytes.clone())

  def fromEnv(name: String): WireAuth = Hmac(s"the environment variable $name", () =>
    sys.env.get(name).map(_.getBytes(UTF_8))
      .getOrElse(throw IllegalStateException(s"the wire secret's environment variable $name is not set")))

  /** the file's bytes, a trailing newline dropped (what `echo` and editors add) */
  def fromFile(path: java.nio.file.Path): WireAuth = Hmac(s"the file $path", () =>
    val b = java.nio.file.Files.readAllBytes(path)
    val end = b.lastIndexWhere(c => c != '\n' && c != '\r') + 1
    b.take(end))

  /** HMAC-SHA256 of `message` under `key`, as lower-case hex */
  def mac(key: Array[Byte], message: String): String =
    val m = javax.crypto.Mac.getInstance("HmacSHA256")
    m.init(javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(message.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString

  /** 16 random bytes, hex */
  def nonce(): String =
    val b = new Array[Byte](16)
    java.security.SecureRandom().nextBytes(b)
    b.map(x => f"${x & 0xff}%02x").mkString

  /** equal in constant time: a mac compared byte by byte leaks, through
   * timing, how much of a guess was right */
  def same(a: String, b: String): Boolean =
    java.security.MessageDigest.isEqual(a.getBytes(UTF_8), b.getBytes(UTF_8))

/**
 * Whether a TCP link is encrypted (polyglot-one-wire stage 5b, wire-tls):
 *
 * {{{
 * given WireSecurity = WireSecurity.tls(WireSecurity.Trust.pem(Path.of("ca.pem")))
 * }}}
 *
 * TLS, with the server's certificate checked against the trust the given
 * names and its name checked against the host dialled (HTTPS rules). Each
 * trust is its own source: a PEM file, a path in an environment variable,
 * or the JDK's own store. The default is plain TCP, as before; pipes and
 * in-process links take no TLS (nothing is between the two ends).
 */
sealed trait WireSecurity

object WireSecurity:
  given plain: WireSecurity = Plain

  case object Plain extends WireSecurity

  final class Tls private[WireSecurity] (val trust: Trust) extends WireSecurity:
    /** the client context, built when a link is opened (a trust whose file
     * is missing fails THEN, by name) */
    def context(): javax.net.ssl.SSLContext =
      val tmf = javax.net.ssl.TrustManagerFactory.getInstance(javax.net.ssl.TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(trust.keyStore())
      val ctx = javax.net.ssl.SSLContext.getInstance("TLS")
      ctx.init(null, tmf.getTrustManagers, null)
      ctx

  def tls(trust: Trust): WireSecurity = Tls(trust)

  /** what a TLS client believes: each its own source */
  sealed trait Trust:
    def source: String
    /** None: the JDK's default store */
    private[WireSecurity] def keyStore(): java.security.KeyStore | Null

  object Trust:
    /** the JDK's own store: servers with certificates from a public CA */
    val system: Trust = new Trust:
      def source = "the JDK's trust store"
      private[WireSecurity] def keyStore(): java.security.KeyStore | Null = null

    /** the certificates in a PEM file: a private CA, or the server's own
     * self-signed certificate */
    def pem(path: java.nio.file.Path): Trust = new Trust:
      def source = s"the PEM file $path"
      private[WireSecurity] def keyStore(): java.security.KeyStore | Null =
        if !java.nio.file.Files.isReadable(path) then
          throw IllegalStateException(s"the TLS trust file $path is not readable")
        val in = java.nio.file.Files.newInputStream(path)
        val certs =
          try java.security.cert.CertificateFactory.getInstance("X.509").generateCertificates(in)
          finally in.close()
        if certs.isEmpty then throw IllegalStateException(s"the TLS trust file $path holds no certificate")
        val ks = java.security.KeyStore.getInstance(java.security.KeyStore.getDefaultType)
        ks.load(null, null)
        var i = 0
        certs.forEach { c => ks.setCertificateEntry(s"okay-trust-$i", c); i += 1 }
        ks

    /** a PEM file whose path is in an environment variable */
    def pemFromEnv(name: String): Trust = new Trust:
      def source = s"the PEM file named by the environment variable $name"
      private[WireSecurity] def keyStore(): java.security.KeyStore | Null =
        val path = sys.env.getOrElse(name,
          throw IllegalStateException(s"the TLS trust's environment variable $name is not set"))
        pem(java.nio.file.Path.of(path)).keyStore()

/**
 * How long an engine waits for an answer (polyglot-one-wire stage 6):
 *
 * {{{
 * given WireDeadline = WireDeadline.after(5.seconds)
 * }}}
 *
 * On a stream link (pipes, TCP) an answer that does not come in time CLOSES
 * the link, which is the only way to abandon a blocked read. The call answers
 * `Left(Condition("timeout", ...))`, and the engine is dead afterwards: a
 * supervisor (`ForeignWorker.supervised`) opens a fresh one. An in-process
 * link cannot be abandoned mid-call, so a deadline there is refused.
 * The default is no deadline, as before.
 */
final case class WireDeadline(millis: Option[Long])

object WireDeadline:
  given none: WireDeadline = WireDeadline(Option.empty)
  def after(d: scala.concurrent.duration.FiniteDuration): WireDeadline = WireDeadline(Some(d.toMillis))

/** a wire line, read STRICTLY (see `whole`) */
object WireJson:
  /**
   * `Json.parse` is total: it repairs damaged text and answers what it could
   * read, which is right for a document a person wrote and wrong for a
   * protocol line — a reply cut short (a truncated FFM read, a dropped TCP
   * tail) must not be read as a smaller reply. Found by in-process-worker's
   * mutant: an answer one byte short passed every test. The fast strict
   * parser first; where it declines, the total one, refused if it repaired.
   */
  def whole(line: String): Json =
    if !balanced(line) then
      throw IllegalStateException(s"the worker's line is not whole JSON (cut short?): ${line.take(200)}")
    JsonValue.parse(line).getOrElse {
      val j = Json.parse(line)
      // a worklist, not a recursion: the parse above takes any depth, so
      // the walk over what it built must too (stack-safety-cbor-edn-wire)
      def damaged(root: Json): Boolean =
        val todo = scala.collection.mutable.Stack[Json](root)
        var hit = false
        while !hit && todo.nonEmpty do todo.pop() match
          case Json.JErr(_) => hit = true
          case Json.JArr(vs) => vs.foreach(todo.push)
          case Json.JObj(fs) => fs.foreach((_, v) => todo.push(v))
          case _ => ()
        hit
      if damaged(j) || !line.trim.endsWith("}") then
        throw IllegalStateException(s"the worker's line is not whole JSON (cut short?): ${line.take(200)}")
      j
    }

  /** every bracket outside a string closed, exactly at the end: a line cut
   * after its inner `}` still parses as a smaller object, and this is the
   * one check such a cut cannot pass */
  private def balanced(line: String): Boolean =
    var depth = 0
    var inString = false
    var escaped = false
    var closedAt = -1
    var i = 0
    val t = line.trim
    while i < t.length do
      val c = t.charAt(i)
      if inString then
        if escaped then escaped = false
        else if c == '\\' then escaped = true
        else if c == '"' then inString = false
      else c match
        case '"' => inString = true
        case '{' | '[' => depth += 1
        case '}' | ']' =>
          depth -= 1
          if depth == 0 then closedAt = i
        case _ => ()
      i += 1
    !inString && depth == 0 && closedAt == t.length - 1

/**
 * The wire on a byte stream: JSON lines until a configure, then frames (a
 * 4-byte big-endian length, then the message). Lines are read byte by byte
 * off the raw stream rather than through a character reader, so a switch
 * from lines to frames loses nothing a reader had buffered ahead.
 */
object WireFrames:
  /** the next line, without its newline; None at the stream's end */
  def readLine(in: BufferedInputStream): Option[String] =
    val b = ByteArrayOutputStream()
    var c = in.read()
    while c != -1 && c != '\n' do
      b.write(c)
      c = in.read()
    if c == -1 && b.size == 0 then None else Some(String(b.toByteArray, UTF_8).stripSuffix("\r"))

  def writeLine(out: OutputStream, line: String): Unit =
    out.write(line.getBytes(UTF_8)); out.write('\n'); out.flush()

  def writeFrame(out: OutputStream, message: Array[Byte]): Unit =
    val n = message.length
    out.write(Array((n >>> 24).toByte, (n >>> 16).toByte, (n >>> 8).toByte, n.toByte))
    out.write(message)
    out.flush()

  /** the next frame; None when the stream ends first */
  def readFrame(in: BufferedInputStream): Option[Array[Byte]] =
    val len = in.readNBytes(4)
    if len.length < 4 then None
    else
      val m = ((len(0) & 0xff) << 24) | ((len(1) & 0xff) << 16) | ((len(2) & 0xff) << 8) | (len(3) & 0xff)
      val body = in.readNBytes(m)
      if body.length < m then None else Some(body)

/**
 * Stage 5's handshake, for any engine: the givens in scope decide the format
 * and the compression. Anything but JSON lines must be ANNOUNCED in the far
 * side's hello (`"speaks":{"format":[..],"compress":[..]}`), then confirmed
 * by a `configure` exchange. An explicit choice the far side did not
 * announce is refused by name; a preference (a compression with a
 * `fallback`) walks its order instead, and is skipped on a link that is
 * not a network one (a pipe, in-process), where it would only cost.
 */
object WireNegotiation:
  /** Right(None): stay on JSON lines; Right(Some(..)): send `configure`;
   * Left: the refusal, naming what the far side speaks */
  def choose(hello: Json, network: Boolean, name: String)
            (using format: WireFormat, compression: WireCompression): Either[String, Option[(WireFormat, WireCompression)]] =
    def announced(key: String): Vector[String] = hello match
      case Json.JObj(fs) => fs.toMap.get("speaks") match
        case Some(Json.JObj(sp)) => sp.toMap.get(key) match
          case Some(Json.JArr(xs)) => xs.collect { case Json.JStr(x) => x }
          // a language that unboxes a one-element list (R's jsonlite)
          case Some(Json.JStr(x)) => Vector(x)
          case _ => Vector.empty
        case _ => Vector.empty
      case _ => Vector.empty
    val formats = "json" +: announced("format")
    val compressions = "none" +: announced("compress")
    @annotation.tailrec
    def pick(c: WireCompression): WireCompression = c.fallback match
      case Some(next) if !network || !compressions.contains(c.name) => pick(next)
      case _ => c
    val chosen = pick(compression)
    if !formats.contains(format.name) then
      Left(s"$name speaks the formats ${formats.distinct.mkString(", ")}; this host's given WireFormat is ${format.name}")
    else if !compressions.contains(chosen.name) then
      Left(s"$name speaks the compressions ${compressions.distinct.mkString(", ")}; this host's given WireCompression is ${chosen.name}")
    else if format.name == "json" && chosen.name == "none" then Right(None)
    else Right(Some((format, chosen)))

  /**
   * The auth step (wire-auth), before any configure: what the hello
   * announced against the given in scope. `roundTrip` sends one JSON line and
   * reads the answer. Right(()) when both sides are content; Left names the
   * refusal.
   */
  def authenticate(hello: Json, name: String, roundTrip: String => Option[Json])
                  (using auth: WireAuth): Either[String, Unit] =
    enum Announced:
      case Nothing
      case Hmac(serverNonce: String)
      case Other(scheme: String)
    val announced = hello match
      case Json.JObj(fs) => fs.toMap.get("auth") match
        case Some(Json.JObj(a)) =>
          val m = a.toMap
          (m.get("scheme"), m.get("nonce")) match
            case (Some(Json.JStr("hmac-sha256")), Some(Json.JStr(ns))) => Announced.Hmac(ns)
            case (Some(Json.JStr(other)), _) => Announced.Other(other)
            case _ => Announced.Other("an unreadable auth announcement")
        case _ => Announced.Nothing
      case _ => Announced.Nothing
    (announced, auth) match
      case (Announced.Nothing, WireAuth.Off) => Right(())
      case (Announced.Other(scheme), _) =>
        Left(s"$name asks for authentication by $scheme; this host speaks hmac-sha256")
      case (Announced.Hmac(_), WireAuth.Off) =>
        Left(s"$name requires hmac-sha256 authentication; this host has no given WireAuth")
      case (Announced.Nothing, h: WireAuth.Hmac) =>
        Left(s"this host's given WireAuth (from ${h.source}) requires $name to authenticate; it announced none")
      case (Announced.Hmac(ns), h: WireAuth.Hmac) =>
        val key = h.key()
        val nc = WireAuth.nonce()
        val ask = Json.print(Json.JObj(Vector("op" -> Json.JStr("auth"), "nonce" -> Json.JStr(nc),
          "mac" -> Json.JStr(WireAuth.mac(key, s"okay-wire client|$ns|$nc")))))
        roundTrip(ask) match
          case None => Left(s"$name closed the wire during authentication")
          case Some(Json.JObj(fs)) => fs.toMap.get("ok") match
            case Some(Json.JObj(ok)) => ok.toMap.get("mac") match
              case Some(Json.JStr(m)) if WireAuth.same(m, WireAuth.mac(key, s"okay-wire server|$ns|$nc")) => Right(())
              case _ => Left(s"$name answered with a mac that does not prove the secret from ${h.source}: refused")
            case _ => Left(s"$name refused this host's authentication (the secret from ${h.source}): ${Json.print(Json.JObj(fs))}")
          case Some(other) => Left(s"$name answered the authentication with ${Json.print(other)}")

  /** the one JSON line that asks the far side to switch */
  def configure(format: WireFormat, compression: WireCompression, arrow: Boolean = false): String =
    Json.print(Json.JObj(Vector("op" -> Json.JStr("configure"), "format" -> Json.JStr(format.name),
      "compress" -> Json.JStr(compression.name)) ++ Option.when(arrow)("frames" -> Json.JStr("arrow"))))

  /** whether frames cross as Arrow: Right(true) when the given asks and the
   * hello announces it; Left when a STRICT Arrow is not spoken */
  def chooseFrames(hello: Json, name: String)(using frames: FrameFormat): Either[String, Boolean] =
    val spoken = hello match
      case Json.JObj(fs) => fs.toMap.get("speaks") match
        case Some(Json.JObj(sp)) => sp.toMap.get("frames") match
          case Some(Json.JArr(xs)) => xs.contains(Json.JStr("arrow"))
          case Some(Json.JStr(x)) => x == "arrow"
          case _ => false
        case _ => false
      case _ => false
    if frames.name != "arrow" then Right(false)
    else if spoken then Right(true)
    else if frames.strict then
      Left(s"$name speaks the frames json; this host's given FrameFormat is arrow (a Python worker announces arrow when pyarrow is installed)")
    else Right(false)

  /** the far side's answer to `configure`: Left names what went wrong */
  def confirmed(name: String, format: WireFormat, compression: WireCompression, answer: Option[Json]): Either[String, Unit] =
    answer match
      case Some(Json.JObj(fs)) if fs.toMap.contains("ok") => Right(())
      case Some(other) => Left(s"$name refused the configuration ${format.name}/${compression.name}: ${Json.print(other)}")
      case None => Left(s"$name closed the wire when asked to configure ${format.name}/${compression.name}")

/**
 * The wire's protocol tree as CBOR (RFC 8949), the subset stage 5a names:
 * integers, float16/32/64, text, definite arrays and maps, true, false,
 * null, undefined (read as null). Anything else is refused by name.
 */
object WireCbor:

  def encode(tree: Json): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    def head(major: Int, n: Long): Unit =
      val m = major << 5
      if n < 24 then out.write(m | n.toInt)
      else if n < 0x100 then { out.write(m | 24); out.write(n.toInt) }
      else if n < 0x10000 then { out.write(m | 25); out.write((n >> 8).toInt); out.write(n.toInt & 0xff) }
      else if n < 0x100000000L then { out.write(m | 26); (3 to 0 by -1).foreach(i => out.write(((n >> (8 * i)) & 0xff).toInt)) }
      else { out.write(m | 27); (7 to 0 by -1).foreach(i => out.write(((n >> (8 * i)) & 0xff).toInt)) }
    // preorder on an explicit stack, children pushed in reverse so they
    // pop in order: a tree as deep as the caller built costs heap, not
    // native stack (stack-safety-cbor-edn-wire)
    val todo = scala.collection.mutable.Stack[Json](tree)
    while todo.nonEmpty do todo.pop() match
      case Json.JNull | Json.JErr(_) => out.write(0xf6)
      case Json.JBool(b) => out.write(if b then 0xf5 else 0xf4)
      case Json.JNum(v) if v.isWhole && math.abs(v) < 9.0e18 =>
        val n = v.toLong
        if n >= 0 then head(0, n) else head(1, -1 - n)
      case Json.JNum(v) =>
        out.write(0xfb)
        val bits = java.lang.Double.doubleToLongBits(v)
        (7 to 0 by -1).foreach(i => out.write(((bits >> (8 * i)) & 0xff).toInt))
      case Json.JStr(s) =>
        val b = s.getBytes(UTF_8)
        head(3, b.length.toLong)
        out.write(b)
      case Json.JArr(vs) =>
        head(4, vs.length.toLong)
        vs.reverseIterator.foreach(todo.push)
      case Json.JObj(fs) =>
        head(5, fs.length.toLong)
        fs.reverseIterator.foreach { (k, v) => todo.push(v); todo.push(Json.JStr(k)) }
    out.toByteArray

  def decode(bytes: Array[Byte]): Either[String, Json] =
    var at = 0
    def byte(): Int =
      if at >= bytes.length then throw IllegalStateException("a CBOR message ended early (cut short?)")
      val b = bytes(at) & 0xff
      at += 1
      b
    def uint(n: Int): Long = (0 until n).foldLeft(0L)((acc, _) => (acc << 8) | byte())
    def arg(info: Int): Long = info match
      case i if i < 24 => i.toLong
      case 24 => uint(1)
      case 25 => uint(2)
      case 26 => uint(4)
      case 27 => uint(8)
      case other => throw IllegalStateException(s"CBOR: an indefinite or reserved length ($other) is not in the wire's subset")
    def half(h: Int): Double =
      val exp = (h >> 10) & 0x1f
      val mant = h & 0x3ff
      val v = if exp == 0 then mant * math.pow(2, -24)
        else if exp != 31 then (mant + 1024) * math.pow(2, exp - 25)
        else if mant == 0 then Double.PositiveInfinity else Double.NaN
      if (h & 0x8000) != 0 then -v else v
    // one open container on an explicit stack: its remaining count and
    // what it has read so far. A message as deep as the far side sent
    // costs heap, not native stack — and a StackOverflowError is no
    // IllegalStateException, so the recursion this replaces escaped the
    // catch below and took the reading thread (stack-safety-cbor-edn-wire)
    final class Open(var left: Long, val arr: Boolean):
      val items = Vector.newBuilder[Json]
      val fields = Vector.newBuilder[(String, Json)]
      var key: String | Null = null
    def count(info: Int): Long =
      val n = arg(info)
      // every item is at least one byte, so a count past what is left is
      // damage however it is read (and never a two-billion-slot builder)
      if n > bytes.length - at then throw IllegalStateException("a CBOR container ended early (cut short?)")
      n
    def item(): Json =
      val open = scala.collection.mutable.Stack[Open]()
      var done: Json | Null = null
      while done == null do
        // a leaf, or null when `scalar` opened a container instead
        val v = scalar(open)
        if v != null then
          var up: Json | Null = v
          // hand the value to its container; a container it completes is
          // the next value handed up
          while up != null do
            if open.isEmpty then { done = up; up = null }
            else
              val o = open.top
              val x: Json = up.nn
              up = null
              if o.arr then { o.items += x; o.left -= 1 }
              else if o.key == null then x match
                case Json.JStr(k) => o.key = k
                case other => throw IllegalStateException(s"CBOR: a map key that is not text: $other")
              else { o.fields += ((o.key.nn, x)); o.key = null; o.left -= 1 }
              if o.left == 0 && o.key == null then
                val closed = open.pop()
                up = if closed.arr then Json.JArr(closed.items.result()) else Json.JObj(closed.fields.result())
      done.nn
    /** one head: a leaf's value, or null after pushing the container it
      * opened (an EMPTY container is a leaf: nothing will complete it) */
    def scalar(open: scala.collection.mutable.Stack[Open]): Json | Null =
      val ib = byte()
      val major = ib >> 5
      val info = ib & 0x1f
      major match
        case 0 => Json.JNum(arg(info).toDouble)
        case 1 => Json.JNum((-1 - arg(info)).toDouble)
        case 3 =>
          val n = arg(info).toInt
          if at + n > bytes.length then throw IllegalStateException("a CBOR string ended early (cut short?)")
          val s = String(bytes, at, n, UTF_8)
          at += n
          Json.JStr(s)
        case 4 =>
          val n = count(info)
          if n == 0 then Json.JArr(Vector.empty) else { open.push(Open(n, arr = true)); null }
        case 5 =>
          val n = count(info)
          if n == 0 then Json.JObj(Vector.empty) else { open.push(Open(n, arr = false)); null }
        case 7 => info match
          case 20 => Json.JBool(false)
          case 21 => Json.JBool(true)
          case 22 | 23 => Json.JNull
          case 25 => Json.JNum(half(uint(2).toInt))
          case 26 => Json.JNum(java.lang.Float.intBitsToFloat(uint(4).toInt).toDouble)
          case 27 => Json.JNum(java.lang.Double.longBitsToDouble(uint(8)))
          case other => throw IllegalStateException(s"CBOR: simple value $other is not in the wire's subset")
        case other => throw IllegalStateException(s"CBOR: major type $other (byte strings, tags) is not in the wire's subset")
    try
      val j = item()
      if at != bytes.length then Left(s"CBOR: ${bytes.length - at} bytes after the message") else Right(j)
    catch case e: IllegalStateException => Left(e.getMessage)
