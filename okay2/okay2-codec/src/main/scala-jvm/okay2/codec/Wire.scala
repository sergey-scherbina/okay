package okay2.codec

import java.io.{BufferedInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * How a wire message's tree becomes bytes (okay-codec's Wire,
 * polyglot-one-wire stage 5), chosen at COMPILE TIME by the implicit in
 * scope where an engine is opened:
 *
 * {{{
 * import okay2.codec.WireFormat.Cbor.cbor           // CBOR instead of JSON
 * import okay2.codec.WireCompression.Off.off        // no compression at all
 * }}}
 *
 * With no import the format is JSON and compression is PREFERRED: deflate,
 * else zlib, else none, whichever the far side announces first, and none
 * on a link that is not a network one. An EXPLICIT choice the far side did
 * not announce is refused by name, never quietly downgraded.
 *
 * The defaults are implicits of the companions, so an import of an
 * explicit one wins by Scala 2's rule that an imported implicit is found
 * before the implicit scope is searched.
 */
trait WireFormat {
  def name: String
  def encode(tree: Json): Array[Byte]
  def decode(bytes: Array[Byte]): Json
}

object WireFormat {
  /** JSON, the default */
  implicit val json: WireFormat = new WireFormat {
    def name = "json"
    def encode(tree: Json): Array[Byte] = Json.print(tree).getBytes(UTF_8)
    def decode(bytes: Array[Byte]): Json = WireJson.whole(new String(bytes, UTF_8))
  }

  /** `import okay2.codec.WireFormat.Cbor.cbor` */
  object Cbor {
    implicit val cbor: WireFormat = new WireFormat {
      def name = "cbor"
      def encode(tree: Json): Array[Byte] = WireCbor.encode(tree)
      def decode(bytes: Array[Byte]): Json = WireCbor.decode(bytes).fold(why => throw new IllegalStateException(why), identity)
    }
  }

  /** the implicits above, chosen at RUNTIME from a name instead of an
   * import — a config value or a flag, for `WireChoice.named` */
  def byName(name: String): Either[String, WireFormat] = name match {
    case "json" => Right(json)
    case "cbor" => Right(Cbor.cbor)
    case other => Left(s"unknown wire format '$other' (json, cbor)")
  }
}

trait WireCompression {
  def name: String
  def compress(bytes: Array[Byte]): Array[Byte]
  def decompress(bytes: Array[Byte]): Array[Byte]
  /** what to try instead where this one is not to be had — a far side
   * that did not announce it, or a link that is not a network one, where
   * it would only cost. None (every explicit choice): refuse by name. */
  def fallback: Option[WireCompression] = None
}

object WireCompression {
  /** THE DEFAULT: an ORDER of preference — raw deflate, then zlib (what R
   * checks natively), then the plain wire — on a NETWORK link only. On a
   * pipe and in-process the plain wire: measured in okay
   * (wire-compression-measured), DEFLATE made a short message longer and
   * every round trip slower, which a pipe's bandwidth never pays back. */
  implicit val preferred: WireCompression = new Zipped(nowrap = true) {
    override def fallback: Option[WireCompression] = Some(new Zipped(nowrap = false) {
      override def fallback: Option[WireCompression] = Some(Off.off)
    })
  }

  /** `import okay2.codec.WireCompression.Off.off`: never compress */
  object Off {
    implicit val off: WireCompression = new WireCompression {
      def name = "none"
      def compress(bytes: Array[Byte]): Array[Byte] = bytes
      def decompress(bytes: Array[Byte]): Array[Byte] = bytes
    }
  }

  /** `import okay2.codec.WireCompression.Deflate.deflate`: raw DEFLATE
   * (RFC 1951) REQUIRED — a far side without it is refused by name, and
   * in-process links compress too */
  object Deflate {
    implicit val deflate: WireCompression = new Zipped(nowrap = true)
  }

  /** `import okay2.codec.WireCompression.Zlib.zlib`: zlib (RFC 1950)
   * REQUIRED */
  object Zlib {
    implicit val zlib: WireCompression = new Zipped(nowrap = false)
  }

  /** the implicits above, chosen at RUNTIME from a name — "auto" is
   * `preferred` */
  def byName(name: String): Either[String, WireCompression] = name match {
    case "auto" => Right(preferred)
    case "none" => Right(Off.off)
    case "deflate" => Right(Deflate.deflate)
    case "zlib" => Right(Zlib.zlib)
    case other => Left(s"unknown wire compression '$other' (auto, none, deflate, zlib)")
  }

  /** DEFLATE from java.util.zip: raw (`nowrap`), which every far side's
   * standard library has, or zlib-wrapped, which R's memCompress is */
  private class Zipped(nowrap: Boolean) extends WireCompression {
    def name: String = if (nowrap) "deflate" else "zlib"
    // a Deflater holds ~256 KB of native zlib state: kept and reset, not
    // made per message (okay's wire-compression-measured). A pool rather
    // than a ThreadLocal, because a virtual thread per call would pin one each.
    private val deflaters = new java.util.concurrent.ConcurrentLinkedQueue[java.util.zip.Deflater]()
    private val inflaters = new java.util.concurrent.ConcurrentLinkedQueue[java.util.zip.Inflater]()

    def compress(bytes: Array[Byte]): Array[Byte] = {
      val d = Option(deflaters.poll()).getOrElse(new java.util.zip.Deflater(java.util.zip.Deflater.DEFAULT_COMPRESSION, nowrap))
      var whole = false
      try {
        d.setInput(bytes)
        d.finish()
        // DEFLATE's worst case is the input plus 5 bytes per 16 KB stored block
        var out = new Array[Byte](bytes.length + bytes.length / 16000 * 5 + 64)
        var n = 0
        while (!d.finished()) {
          if (n == out.length) out = java.util.Arrays.copyOf(out, out.length * 2)
          n += d.deflate(out, n, out.length - n)
        }
        whole = true
        java.util.Arrays.copyOf(out, n)
      } finally {
        if (whole && deflaters.size < Zipped.Kept) { d.reset(); val _ = deflaters.offer(d) }
        else d.end()
      }
    }

    def decompress(bytes: Array[Byte]): Array[Byte] = {
      val i = Option(inflaters.poll()).getOrElse(new java.util.zip.Inflater(nowrap))
      var whole = false
      try {
        i.setInput(bytes)
        var out = new Array[Byte](math.max(64, bytes.length * 4))
        var n = 0
        while (!i.finished()) {
          if (n == out.length) out = java.util.Arrays.copyOf(out, out.length * 2)
          val got = i.inflate(out, n, out.length - n)
          // an empty message finishes on a step that yields nothing
          if (got == 0 && !i.finished() && (i.needsInput() || i.needsDictionary()))
            throw new IllegalStateException("a DEFLATE message ended before its data did (cut short?)")
          n += got
        }
        whole = true
        java.util.Arrays.copyOf(out, n)
      } finally {
        // one that refused a message is not trusted with the next
        if (whole && inflaters.size < Zipped.Kept) { i.reset(); val _ = inflaters.offer(i) }
        else i.end()
      }
    }
  }

  private object Zipped {
    /** per direction, per compression: a far side is one exchange at a time */
    val Kept = 4
  }
}

/**
 * How a FRAME crosses: as the wire's JSON frame, or as an Arrow IPC stream
 * where the far side announces `"frames":["arrow"]`. The default prefers
 * Arrow and never refuses; `Json.json` never uses it; `Arrow.arrow`
 * requires it.
 */
final case class FrameFormat(name: String, strict: Boolean)

object FrameFormat {
  /** THE DEFAULT: a preference, not a demand */
  implicit val preferred: FrameFormat = FrameFormat("arrow", strict = false)
  object Json {
    implicit val json: FrameFormat = FrameFormat("json", strict = true)
  }
  object Arrow {
    implicit val arrow: FrameFormat = FrameFormat("arrow", strict = true)
  }

  /** the implicits above, chosen at RUNTIME from a name — "auto" is `preferred` */
  def byName(name: String): Either[String, FrameFormat] = name match {
    case "auto" => Right(preferred)
    case "json" => Right(Json.json)
    case "arrow" => Right(Arrow.arrow)
    case other => Left(s"unknown frame format '$other' (auto, json, arrow)")
  }
}

/**
 * `WireFormat`, `WireCompression` and `FrameFormat` as one EXPLICIT value,
 * for a caller that picks the wire at RUNTIME — from a flag or a config
 * file — beside the implicits, not instead of them:
 *
 * {{{
 * WireChoice.named(format = "cbor", frames = "json")   // Left on a bad name
 * }}}
 */
final case class WireChoice(format: WireFormat = WireFormat.json,
                            compression: WireCompression = WireCompression.preferred,
                            frames: FrameFormat = FrameFormat.preferred,
                            deadline: WireDeadline = WireDeadline.none)

object WireChoice {
  /** the same wire the implicit defaults pick with no import at all */
  val default: WireChoice = WireChoice()

  /** each name one of its `byName`'s; a name outside them is a `Left`
   * naming it and its choices, never a silent fallback */
  def named(format: String = "json", compression: String = "auto", frames: String = "auto"): Either[String, WireChoice] =
    for {
      f <- WireFormat.byName(format)
      c <- WireCompression.byName(compression)
      fr <- FrameFormat.byName(frames)
    } yield WireChoice(f, c, fr)
}

/**
 * Who may speak on the wire (wire-auth): a MUTUAL HMAC-SHA256 challenge at
 * the handshake. Each side proves it holds the secret without sending it.
 * The secret comes from the source the implicit names, read when a worker
 * is opened:
 *
 * {{{
 * implicit val auth: WireAuth = WireAuth.fromEnv("OKAY_WIRE_SECRET")
 * }}}
 *
 * No implicit means no authentication. It does not encrypt: TLS is the
 * layer for that, and the two compose.
 */
sealed trait WireAuth {
  /** where the secret comes from, for a refusal to name */
  def source: String
}

object WireAuth {
  /** the default: no authentication */
  implicit val none: WireAuth = Off

  case object Off extends WireAuth {
    def source = "no implicit WireAuth"
  }

  /** a secret, read when a worker is opened; a source that has none fails
   * THEN, by name, rather than authenticating with an empty key */
  final class Hmac private[WireAuth] (val source: String, read: () => Array[Byte]) extends WireAuth {
    def key(): Array[Byte] = {
      val k = read()
      if (k.isEmpty) throw new IllegalStateException(s"the wire secret from $source is empty")
      k
    }
  }

  def secret(bytes: Array[Byte]): WireAuth = new Hmac("a secret in the program", () => bytes.clone())

  def fromEnv(name: String): WireAuth = new Hmac(s"the environment variable $name", () =>
    sys.env.get(name).map(_.getBytes(UTF_8))
      .getOrElse(throw new IllegalStateException(s"the wire secret's environment variable $name is not set")))

  /** the file's bytes, a trailing newline dropped (what `echo` and editors add) */
  def fromFile(path: java.nio.file.Path): WireAuth = new Hmac(s"the file $path", () => {
    val b = java.nio.file.Files.readAllBytes(path)
    val end = b.lastIndexWhere(c => c != '\n' && c != '\r') + 1
    b.take(end)
  })

  /** HMAC-SHA256 of `message` under `key`, as lower-case hex */
  def mac(key: Array[Byte], message: String): String = {
    val m = javax.crypto.Mac.getInstance("HmacSHA256")
    m.init(new javax.crypto.spec.SecretKeySpec(key, "HmacSHA256"))
    m.doFinal(message.getBytes(UTF_8)).map(b => f"${b & 0xff}%02x").mkString
  }

  /** 16 random bytes, hex */
  def nonce(): String = {
    val b = new Array[Byte](16)
    new java.security.SecureRandom().nextBytes(b)
    b.map(x => f"${x & 0xff}%02x").mkString
  }

  /** equal in constant time: a mac compared byte by byte leaks, through
   * timing, how much of a guess was right */
  def same(a: String, b: String): Boolean =
    java.security.MessageDigest.isEqual(a.getBytes(UTF_8), b.getBytes(UTF_8))
}

/**
 * Whether a TCP link is encrypted (wire-tls): TLS, with the server's
 * certificate checked against the trust the implicit names and its name
 * against the host dialled. The default is plain TCP.
 *
 * {{{
 * implicit val sec: WireSecurity = WireSecurity.tls(WireSecurity.Trust.pem(Path.of("ca.pem")))
 * }}}
 */
sealed trait WireSecurity

object WireSecurity {
  implicit val plain: WireSecurity = Plain

  case object Plain extends WireSecurity

  final class Tls private[WireSecurity] (val trust: Trust) extends WireSecurity {
    /** the client context, built when a link is opened (a trust whose file
     * is missing fails THEN, by name) */
    def context(): javax.net.ssl.SSLContext = {
      val tmf = javax.net.ssl.TrustManagerFactory.getInstance(javax.net.ssl.TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(trust.keyStore().orNull)
      val ctx = javax.net.ssl.SSLContext.getInstance("TLS")
      ctx.init(null, tmf.getTrustManagers, null)
      ctx
    }
  }

  def tls(trust: Trust): WireSecurity = new Tls(trust)

  /** what a TLS client believes: each its own source */
  sealed trait Trust {
    def source: String
    /** None: the JDK's default store */
    private[WireSecurity] def keyStore(): Option[java.security.KeyStore]
  }

  object Trust {
    /** the JDK's own store: servers with certificates from a public CA */
    val system: Trust = new Trust {
      def source = "the JDK's trust store"
      private[WireSecurity] def keyStore(): Option[java.security.KeyStore] = None
    }

    /** the certificates in a PEM file: a private CA, or the server's own
     * self-signed certificate */
    def pem(path: java.nio.file.Path): Trust = new Trust {
      def source = s"the PEM file $path"
      private[WireSecurity] def keyStore(): Option[java.security.KeyStore] = {
        if (!java.nio.file.Files.isReadable(path))
          throw new IllegalStateException(s"the TLS trust file $path is not readable")
        val in = java.nio.file.Files.newInputStream(path)
        val certs =
          try java.security.cert.CertificateFactory.getInstance("X.509").generateCertificates(in)
          finally in.close()
        if (certs.isEmpty) throw new IllegalStateException(s"the TLS trust file $path holds no certificate")
        val ks = java.security.KeyStore.getInstance(java.security.KeyStore.getDefaultType)
        ks.load(null, null)
        var i = 0
        certs.forEach { c => ks.setCertificateEntry(s"okay-trust-$i", c); i += 1 }
        Some(ks)
      }
    }

    /** a PEM file whose path is in an environment variable */
    def pemFromEnv(name: String): Trust = new Trust {
      def source = s"the PEM file named by the environment variable $name"
      private[WireSecurity] def keyStore(): Option[java.security.KeyStore] = {
        val path = sys.env.getOrElse(name,
          throw new IllegalStateException(s"the TLS trust's environment variable $name is not set"))
        pem(java.nio.file.Path.of(path)).keyStore()
      }
    }
  }
}

/**
 * How long an engine waits for an answer (polyglot-one-wire stage 6): on a
 * stream link an answer that does not come in time CLOSES the link; an
 * in-process link cannot be abandoned mid-call, so a deadline there is
 * refused. The default is no deadline.
 */
final case class WireDeadline(millis: Option[Long])

object WireDeadline {
  implicit val none: WireDeadline = WireDeadline(Option.empty)
  def after(d: scala.concurrent.duration.FiniteDuration): WireDeadline = WireDeadline(Some(d.toMillis))
}

/** a wire line, read STRICTLY (see `whole`) */
object WireJson {
  /**
   * `Json.parse` is total: it repairs damaged text, which is right for a
   * document a person wrote and wrong for a protocol line — a reply cut
   * short must not be read as a smaller reply. The fast strict parser
   * first; where it declines, the total one, refused if it repaired.
   */
  def whole(line: String): Json = {
    if (!balanced(line))
      throw new IllegalStateException(s"the worker's line is not whole JSON (cut short?): ${line.take(200)}")
    JsonValue.parse(line).getOrElse {
      val j = Json.parse(line)
      if (damaged(j) || !line.trim.endsWith("}"))
        throw new IllegalStateException(s"the worker's line is not whole JSON (cut short?): ${line.take(200)}")
      j
    }
  }

  /** a worklist, not a recursion: the parse takes any depth, so the walk
   * over what it built must too */
  private def damaged(root: Json): Boolean = {
    val todo = scala.collection.mutable.Stack[Json](root)
    var hit = false
    while (!hit && todo.nonEmpty) todo.pop() match {
      case Json.JErr(_) => hit = true
      case Json.JArr(vs) => vs.foreach(todo.push)
      case Json.JObj(fs) => fs.foreach(f => todo.push(f._2))
      case _ => ()
    }
    hit
  }

  /** every bracket outside a string closed, exactly at the end: a line cut
   * after its inner `}` still parses as a smaller object, and this is the
   * one check such a cut cannot pass */
  private def balanced(line: String): Boolean = {
    var depth = 0
    var inString = false
    var escaped = false
    var closedAt = -1
    var i = 0
    val t = line.trim
    while (i < t.length) {
      val c = t.charAt(i)
      if (inString) {
        if (escaped) escaped = false
        else if (c == '\\') escaped = true
        else if (c == '"') inString = false
      }
      else c match {
        case '"' => inString = true
        case '{' | '[' => depth += 1
        case '}' | ']' =>
          depth -= 1
          if (depth == 0) closedAt = i
        case _ => ()
      }
      i += 1
    }
    !inString && depth == 0 && closedAt == t.length - 1
  }
}

/**
 * The wire on a byte stream: JSON lines until a configure, then frames (a
 * 4-byte big-endian length, then the message). Lines are read byte by
 * byte off the raw stream, so a switch from lines to frames loses nothing
 * a reader had buffered ahead.
 */
object WireFrames {
  /** the next line, without its newline; None at the stream's end */
  def readLine(in: BufferedInputStream): Option[String] = {
    val b = new ByteArrayOutputStream()
    var c = in.read()
    while (c != -1 && c != '\n') {
      b.write(c)
      c = in.read()
    }
    if (c == -1 && b.size == 0) None else Some(new String(b.toByteArray, UTF_8).stripSuffix("\r"))
  }

  def writeLine(out: OutputStream, line: String): Unit = {
    out.write(line.getBytes(UTF_8)); out.write('\n'.toInt); out.flush()
  }

  def writeFrame(out: OutputStream, message: Array[Byte]): Unit = {
    val n = message.length
    out.write(Array((n >>> 24).toByte, (n >>> 16).toByte, (n >>> 8).toByte, n.toByte))
    out.write(message)
    out.flush()
  }

  /** the next frame; None when the stream ends first */
  def readFrame(in: BufferedInputStream): Option[Array[Byte]] = {
    val len = in.readNBytes(4)
    if (len.length < 4) None
    else {
      val m = ((len(0) & 0xff) << 24) | ((len(1) & 0xff) << 16) | ((len(2) & 0xff) << 8) | (len(3) & 0xff)
      val body = in.readNBytes(m)
      if (body.length < m) None else Some(body)
    }
  }
}

/**
 * The handshake, for any engine: the implicits in scope decide the format
 * and the compression. Anything but JSON lines must be ANNOUNCED in the
 * far side's hello (`"speaks":{"format":[..],"compress":[..]}`), then
 * confirmed by a `configure` exchange. An explicit choice the far side did
 * not announce is refused by name; a preference (a compression with a
 * `fallback`) walks its order instead, and is skipped on a link that is
 * not a network one.
 */
object WireNegotiation {

  private def field(j: Json, key: String): Option[Json] = j match {
    case Json.JObj(fs) => fs.collectFirst { case (k, v) if k == key => v }
    case _ => None
  }

  /** Right(None): stay on JSON lines; Right(Some(..)): send `configure`;
   * Left: the refusal, naming what the far side speaks */
  def choose(hello: Json, network: Boolean, name: String)
            (implicit format: WireFormat, compression: WireCompression): Either[String, Option[(WireFormat, WireCompression)]] = {
    def announced(key: String): Vector[String] = field(hello, "speaks").flatMap(field(_, key)) match {
      case Some(Json.JArr(xs)) => xs.collect { case Json.JStr(x) => x }
      // a language that unboxes a one-element list (R's jsonlite)
      case Some(Json.JStr(x)) => Vector(x)
      case _ => Vector.empty
    }
    val formats = "json" +: announced("format")
    val compressions = "none" +: announced("compress")
    @annotation.tailrec
    def pick(c: WireCompression): WireCompression = c.fallback match {
      case Some(next) if !network || !compressions.contains(c.name) => pick(next)
      case _ => c
    }
    val chosen = pick(compression)
    if (!formats.contains(format.name))
      Left(s"$name speaks the formats ${formats.distinct.mkString(", ")}; this host's implicit WireFormat is ${format.name}")
    else if (!compressions.contains(chosen.name))
      Left(s"$name speaks the compressions ${compressions.distinct.mkString(", ")}; this host's implicit WireCompression is ${chosen.name}")
    else if (format.name == "json" && chosen.name == "none") Right(None)
    else Right(Some((format, chosen)))
  }

  /** what a hello announced about authentication */
  private sealed trait Announced
  private case object NoAuth extends Announced
  private final case class HmacAuth(serverNonce: String) extends Announced
  private final case class OtherAuth(scheme: String) extends Announced

  /**
   * The auth step, before any configure: what the hello announced against
   * the implicit in scope. `roundTrip` sends one JSON line and reads the
   * answer. Right(()) when both sides are content; Left names the refusal.
   */
  def authenticate(hello: Json, name: String, roundTrip: String => Option[Json])
                  (implicit auth: WireAuth): Either[String, Unit] = {
    val announced: Announced = field(hello, "auth") match {
      case Some(a: Json.JObj) => (field(a, "scheme"), field(a, "nonce")) match {
        case (Some(Json.JStr("hmac-sha256")), Some(Json.JStr(ns))) => HmacAuth(ns)
        case (Some(Json.JStr(other)), _) => OtherAuth(other)
        case _ => OtherAuth("an unreadable auth announcement")
      }
      case _ => NoAuth
    }
    (announced, auth) match {
      case (NoAuth, WireAuth.Off) => Right(())
      case (OtherAuth(scheme), _) =>
        Left(s"$name asks for authentication by $scheme; this host speaks hmac-sha256")
      case (HmacAuth(_), WireAuth.Off) =>
        Left(s"$name requires hmac-sha256 authentication; this host has no implicit WireAuth")
      case (NoAuth, h: WireAuth.Hmac) =>
        Left(s"this host's implicit WireAuth (from ${h.source}) requires $name to authenticate; it announced none")
      case (HmacAuth(ns), h: WireAuth.Hmac) =>
        val key = h.key()
        val nc = WireAuth.nonce()
        val ask = Json.print(Json.JObj(Vector("op" -> Json.JStr("auth"), "nonce" -> Json.JStr(nc),
          "mac" -> Json.JStr(WireAuth.mac(key, s"okay-wire client|$ns|$nc")))))
        roundTrip(ask) match {
          case None => Left(s"$name closed the wire during authentication")
          case Some(answer: Json.JObj) => field(answer, "ok") match {
            case Some(ok: Json.JObj) => field(ok, "mac") match {
              case Some(Json.JStr(m)) if WireAuth.same(m, WireAuth.mac(key, s"okay-wire server|$ns|$nc")) => Right(())
              case _ => Left(s"$name answered with a mac that does not prove the secret from ${h.source}: refused")
            }
            case _ => Left(s"$name refused this host's authentication (the secret from ${h.source}): ${Json.print(answer)}")
          }
          case Some(other) => Left(s"$name answered the authentication with ${Json.print(other)}")
        }
    }
  }

  /** the one JSON line that asks the far side to switch */
  def configure(format: WireFormat, compression: WireCompression, arrow: Boolean = false): String =
    Json.print(Json.JObj(Vector("op" -> Json.JStr("configure"), "format" -> Json.JStr(format.name),
      "compress" -> Json.JStr(compression.name)) ++ (if (arrow) Vector("frames" -> Json.JStr("arrow")) else Vector.empty)))

  /** whether frames cross as Arrow: Right(true) when the implicit asks and
   * the hello announces it; Left when a STRICT Arrow is not spoken */
  def chooseFrames(hello: Json, name: String)(implicit frames: FrameFormat): Either[String, Boolean] = {
    val spoken = field(hello, "speaks").flatMap(field(_, "frames")) match {
      case Some(Json.JArr(xs)) => xs.contains(Json.JStr("arrow"))
      case Some(Json.JStr(x)) => x == "arrow"
      case _ => false
    }
    if (frames.name != "arrow") Right(false)
    else if (spoken) Right(true)
    else if (frames.strict)
      Left(s"$name speaks the frames json; this host's implicit FrameFormat is arrow (a Python worker announces arrow when pyarrow is installed)")
    else Right(false)
  }

  /** the far side's answer to `configure`: Left names what went wrong */
  def confirmed(name: String, format: WireFormat, compression: WireCompression, answer: Option[Json]): Either[String, Unit] =
    answer match {
      case Some(a: Json.JObj) if field(a, "ok").isDefined => Right(())
      case Some(other) => Left(s"$name refused the configuration ${format.name}/${compression.name}: ${Json.print(other)}")
      case None => Left(s"$name closed the wire when asked to configure ${format.name}/${compression.name}")
    }
}

/**
 * The wire's protocol tree as CBOR (RFC 8949), the subset the wire names:
 * integers, float16/32/64, text, definite arrays and maps, true, false,
 * null, undefined (read as null). Anything else is refused by name. Both
 * directions walk an explicit stack, so a tree as deep as the far side
 * sent costs heap, not native stack.
 */
object WireCbor {

  def encode(tree: Json): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream()
    def head(major: Int, n: Long): Unit = {
      val m = major << 5
      if (n < 24) out.write(m | n.toInt)
      else if (n < 0x100) { out.write(m | 24); out.write(n.toInt) }
      else if (n < 0x10000) { out.write(m | 25); out.write((n >> 8).toInt); out.write(n.toInt & 0xff) }
      else if (n < 0x100000000L) { out.write(m | 26); (3 to 0 by -1).foreach(i => out.write(((n >> (8 * i)) & 0xff).toInt)) }
      else { out.write(m | 27); (7 to 0 by -1).foreach(i => out.write(((n >> (8 * i)) & 0xff).toInt)) }
    }
    // preorder on an explicit stack, children pushed in reverse so they pop in order
    val todo = scala.collection.mutable.Stack[Json](tree)
    while (todo.nonEmpty) todo.pop() match {
      case Json.JNull | Json.JErr(_) => out.write(0xf6)
      case Json.JBool(b) => out.write(if (b) 0xf5 else 0xf4)
      case Json.JNum(v) if v.isWhole && math.abs(v) < 9.0e18 =>
        val n = v.toLong
        if (n >= 0) head(0, n) else head(1, -1 - n)
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
        fs.reverseIterator.foreach { case (k, v) => todo.push(v); todo.push(Json.JStr(k)) }
    }
    out.toByteArray
  }

  /** one open container on the decoder's explicit stack: its remaining
   * count, what it has read so far, and a map's pending key */
  private final class Open(var left: Long, val arr: Boolean) {
    val items = Vector.newBuilder[Json]
    val fields = Vector.newBuilder[(String, Json)]
    var key: Option[String] = None
  }

  def decode(bytes: Array[Byte]): Either[String, Json] = {
    var at = 0
    def byte(): Int = {
      if (at >= bytes.length) throw new IllegalStateException("a CBOR message ended early (cut short?)")
      val b = bytes(at) & 0xff
      at += 1
      b
    }
    def uint(n: Int): Long = (0 until n).foldLeft(0L)((acc, _) => (acc << 8) | byte().toLong)
    def arg(info: Int): Long = info match {
      case i if i < 24 => i.toLong
      case 24 => uint(1)
      case 25 => uint(2)
      case 26 => uint(4)
      case 27 => uint(8)
      case other => throw new IllegalStateException(s"CBOR: an indefinite or reserved length ($other) is not in the wire's subset")
    }
    def half(h: Int): Double = {
      val exp = (h >> 10) & 0x1f
      val mant = h & 0x3ff
      val v = if (exp == 0) mant * math.pow(2, -24)
        else if (exp != 31) (mant + 1024) * math.pow(2, (exp - 25).toDouble)
        else if (mant == 0) Double.PositiveInfinity else Double.NaN
      if ((h & 0x8000) != 0) -v else v
    }
    def count(info: Int): Long = {
      val n = arg(info)
      // every item is at least one byte, so a count past what is left is
      // damage however it is read (and never a two-billion-slot builder)
      if (n > bytes.length - at) throw new IllegalStateException("a CBOR container ended early (cut short?)")
      n
    }
    val open = scala.collection.mutable.Stack[Open]()
    /** one head: a leaf's value, or None after pushing the container it
     * opened (an EMPTY container is a leaf: nothing will complete it) */
    def scalar(): Option[Json] = {
      val ib = byte()
      val major = ib >> 5
      val info = ib & 0x1f
      major match {
        case 0 => Some(Json.JNum(arg(info).toDouble))
        case 1 => Some(Json.JNum((-1 - arg(info)).toDouble))
        case 3 =>
          val n = arg(info).toInt
          if (n < 0 || at + n > bytes.length) throw new IllegalStateException("a CBOR string ended early (cut short?)")
          val s = new String(bytes, at, n, UTF_8)
          at += n
          Some(Json.JStr(s))
        case 4 =>
          val n = count(info)
          if (n == 0) Some(Json.JArr(Vector.empty)) else { open.push(new Open(n, arr = true)); None }
        case 5 =>
          val n = count(info)
          if (n == 0) Some(Json.JObj(Vector.empty)) else { open.push(new Open(n, arr = false)); None }
        case 7 => info match {
          case 20 => Some(Json.JBool(false))
          case 21 => Some(Json.JBool(true))
          case 22 | 23 => Some(Json.JNull)
          case 25 => Some(Json.JNum(half(uint(2).toInt)))
          case 26 => Some(Json.JNum(java.lang.Float.intBitsToFloat(uint(4).toInt).toDouble))
          case 27 => Some(Json.JNum(java.lang.Double.longBitsToDouble(uint(8))))
          case other => throw new IllegalStateException(s"CBOR: simple value $other is not in the wire's subset")
        }
        case other => throw new IllegalStateException(s"CBOR: major type $other (byte strings, tags) is not in the wire's subset")
      }
    }
    def item(): Json = {
      var done: Option[Json] = None
      while (done.isEmpty) {
        var up = scalar()
        // hand the value to its container; a container it completes is the
        // next value handed up
        while (up.isDefined) {
          val x = up.get
          up = None
          if (open.isEmpty) done = Some(x)
          else {
            val o = open.top
            if (o.arr) { o.items += x; o.left -= 1 }
            else o.key match {
              case None => x match {
                case Json.JStr(k) => o.key = Some(k)
                case other => throw new IllegalStateException(s"CBOR: a map key that is not text: $other")
              }
              case Some(k) => o.fields += ((k, x)); o.key = None; o.left -= 1
            }
            if (o.left == 0 && o.key.isEmpty) {
              val closed = open.pop()
              up = Some(if (closed.arr) Json.JArr(closed.items.result()) else Json.JObj(closed.fields.result()))
            }
          }
        }
      }
      done.get
    }
    try {
      val j = item()
      if (at != bytes.length) Left(s"CBOR: ${bytes.length - at} bytes after the message") else Right(j)
    } catch { case e: IllegalStateException => Left(e.getMessage) }
  }
}
