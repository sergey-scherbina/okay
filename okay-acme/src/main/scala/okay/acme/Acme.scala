package okay.acme

import okay.*
import okay.given
import okay.codec.Json
import okay.http.{Body, Http, Method, Request, Response}
import okay.security.{Crypto, Es256}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.{KeyPair, KeyPairGenerator, PublicKey}
import java.security.interfaces.ECPublicKey
import java.security.spec.{ECGenParameterSpec, PKCS8EncodedKeySpec, X509EncodedKeySpec}

/**
 * An ACME client (RFC 8555) — the protocol a certificate authority
 * speaks to issue a certificate without a human: prove you control
 * the name, receive 90 days of certificate, renew before it runs out.
 *
 * Deliberately narrow, and the narrowness is the point. HTTP-01 only
 * (this stack owns a server; DNS-01 would ask for a DNS provider
 * this repository has no seam for). One order over a list of names.
 * An EC account key, kept as PKCS#8 beside the certificate it earns.
 * The STAGING directory by default, because a first run that burns a
 * production rate limit is a mistake you wait a week to undo.
 *
 * What it is NOT: a certificate manager. It issues and renews one
 * identity for one server; a fleet wants certbot or a proxy, and
 * this module says so rather than growing into one.
 *
 * The two things a caller supplies: an `Http` (the client half of
 * whatever transport it runs) and a `Challenges` — somewhere to put
 * the token the CA will come back for. `okay.script.Serve` answers
 * both from what it already has.
 */
object Acme:

  /** where the CA's own map of endpoints lives */
  object Directory:
    val letsEncryptStaging = "https://acme-staging-v02.api.letsencrypt.org/directory"
    val letsEncrypt = "https://acme-v02.api.letsencrypt.org/directory"

  final case class Config(
    email: String,
    domains: Vector[String],
    /** where the account key, the certificate and its key live */
    accountKey: Path,
    certFile: Path,
    keyFile: Path,
    directory: String = Directory.letsEncryptStaging,
    /** renew when less than this is left of the certificate */
    renewBefore: java.time.Duration = java.time.Duration.ofDays(30),
    /** how long to wait for an authorization or an order to settle */
    timeout: java.time.Duration = java.time.Duration.ofSeconds(60),
    /** External Account Binding (RFC 8555 §7.3.4), when the CA will
     * not open an account without proof you already have one with
     * them: the (key id, base64url MAC key) pair they hand you out of
     * band. Let's Encrypt needs none; most commercial CAs do. */
    eab: Option[(String, String)] = None,
  )

  /** what the CA will fetch: `/.well-known/acme-challenge/<token>`
   * must answer `keyAuthorization` in plain text, from the name the
   * order is for, on port 80 */
  trait Challenges:
    def put(token: String, keyAuthorization: String): Unit
    def remove(token: String): Unit

  object Challenges:
    /** the in-memory one a server serves from -- `routes` is the
     * PartialFunction to chain BEFORE any https redirect, since the
     * CA speaks plain HTTP and follows no redirect for this */
    final class Memory extends Challenges:
      private val held = new java.util.concurrent.ConcurrentHashMap[String, String]
      def put(token: String, keyAuthorization: String): Unit = held.put(token, keyAuthorization): Unit
      def remove(token: String): Unit = held.remove(token): Unit
      def size: Int = held.size

      val Prefix = "/.well-known/acme-challenge/"

      def routes: PartialFunction[Request, Response ! Async] = {
        case r if r.method == Method.Get && path(r.url).startsWith(Prefix) &&
                  held.containsKey(path(r.url).drop(Prefix.length)) =>
          pure(Response(200, Vector("Content-Type" -> "text/plain"),
            Http.one(held.get(path(r.url).drop(Prefix.length)).getBytes(UTF_8))))
      }

      private def path(url: String): String = url.takeWhile(_ != '?')

  /** what a run did -- a value, because "it renewed" and "it was
   * already good" are different things an operator wants told apart */
  enum Outcome:
    case Issued(domains: Vector[String], notAfter: java.time.Instant)
    case Current(notAfter: java.time.Instant)

  /**
   * Issue or renew, as needed: answers `Current` when the certificate
   * on disk is not due yet, and otherwise runs the whole flow and
   * writes the new one. Never throws — a refusal names the step it
   * failed at, because "ACME failed" is not something an operator can
   * act on.
   *
   * Two things decide "due" (acme-ari). `renewBefore` is ours: a
   * countdown to the expiry. The CA's own SUGGESTED WINDOW is the
   * other, and it exists because a mass revocation otherwise becomes
   * every client on earth renewing in the same minute. They are read
   * TOGETHER and either can trigger: a CA that publishes nothing, or
   * publishes nonsense, must never be able to stop a renewal our own
   * countdown wants.
   */
  def ensure(cfg: Config, http: Http, challenges: Challenges)
            (using Crypto, CanBlock): Either[String, Outcome] =
    notAfterOf(cfg.certFile) match
      case Some(t) if java.time.Instant.now().plus(cfg.renewBefore).isBefore(t) &&
                      !windowOpen(cfg, http) => Right(Outcome.Current(t))
      case _ => issue(cfg, http, challenges).flatMap { _ =>
        notAfterOf(cfg.certFile)
          .map(t => Outcome.Issued(cfg.domains, t))
          .toRight("the certificate was written but cannot be read back")
      }

  /**
   * Why a certificate is being taken back (RFC 5280 §5.3.1, the
   * subset RFC 8555 allows). A NAMED reason rather than the integer
   * nobody remembers, because this is the field an incident report
   * quotes: `KeyCompromise` is not a synonym for `Superseded`, and a
   * CA treats them differently -- a compromised key blocks reissue
   * for that key, a superseded one does not.
   */
  enum Reason(val code: Int):
    case Unspecified extends Reason(0)
    case KeyCompromise extends Reason(1)
    case Superseded extends Reason(4)
    case CessationOfOperation extends Reason(5)

  /**
   * Take the certificate back (RFC 8555 §7.6): the leaf's DER, signed
   * by the ACCOUNT key that ordered it. A revoked certificate stops
   * being trusted before it expires -- what a leaked key needs, and
   * the reason this is not just "let it lapse in 90 days".
   *
   * Revoking twice is not an error worth inventing: the CA answers
   * `alreadyRevoked`, and that sentence comes back as it is.
   */
  def revoke(cfg: Config, http: Http, reason: Reason = Reason.Unspecified)
            (using Crypto, CanBlock): Either[String, Unit] =
    for
      leaf <- leafDer(cfg.certFile)
      account <- accountKeyOf(cfg.accountKey)
      dir <- get(http, cfg.directory).flatMap(json)
      newNonce <- str(dir, "newNonce").toRight("the directory has no newNonce")
      newAccount <- str(dir, "newAccount").toRight("the directory has no newAccount")
      revokeCert <- str(dir, "revokeCert").toRight("this CA publishes no revokeCert endpoint")
      session = new Session(http, account, newNonce, cfg.eab)
      // the account must be the one that ordered it, so it registers
      // first -- against an existing account this answers the same URL
      _ <- session.register(newAccount, cfg.email)
      _ <- session.post(revokeCert, Json.print(Json.JObj(Vector(
        "certificate" -> Json.JStr(b64(leaf)),
        "reason" -> Json.JNum(reason.code.toDouble)))))
    yield ()

  /** the LEAF's DER: a chain file holds the issuers too, and a CA
   * revokes one certificate, not a bundle */
  private[acme] def leafDer(certFile: Path): Either[String, Array[Byte]] =
    leafCert(certFile).map(_.getEncoded)

  /** the CA's suggested renewal window for the certificate on disk,
   * when the CA publishes one (RFC 8555's ARI extension). `None` is
   * every ordinary reason it might be missing -- this CA has no
   * renewalInfo, the certificate has no Authority Key Identifier, the
   * answer did not parse -- and none of them is an error worth
   * stopping a renewal for. */
  def renewalWindow(cfg: Config, http: Http)(using CanBlock)
  : Option[(java.time.Instant, java.time.Instant)] =
    for
      dir <- get(http, cfg.directory).flatMap(json).toOption
      endpoint <- str(dir, "renewalInfo")
      id <- certId(cfg.certFile).toOption
      body <- get(http, s"$endpoint/$id").flatMap(json).toOption
      window <- field(body, "suggestedWindow")
      start <- str(window, "start").flatMap(instant)
      end <- str(window, "end").flatMap(instant)
    yield (start, end)

  /** has the CA's window opened? A missing or unreadable window is
   * `false` -- it must not TRIGGER a renewal either, only allow one
   * earlier than our own countdown would */
  private def windowOpen(cfg: Config, http: Http)(using CanBlock): Boolean =
    renewalWindow(cfg, http).exists((start, _) => !java.time.Instant.now().isBefore(start))

  private def instant(s: String): Option[java.time.Instant] =
    try Some(java.time.Instant.parse(s))
    catch case _: Exception =>
      try Some(java.time.OffsetDateTime.parse(s).toInstant) catch case _: Exception => None

  /**
   * The certificate's ARI identifier: base64url of the Authority Key
   * Identifier's keyIdentifier, a dot, base64url of the serial.
   *
   * Both are read out of the leaf's own DER by hand, because the JDK
   * hands the AKI over only as the raw extension bytes: an OCTET
   * STRING wrapping `SEQUENCE { [0] keyIdentifier }`. Three nested
   * unwraps and no more -- a certificate whose AKI is shaped
   * differently answers a refusal rather than a guess.
   */
  private[acme] def certId(certFile: Path): Either[String, String] =
    for
      leaf <- leafCert(certFile)
      raw <- Option(leaf.getExtensionValue("2.5.29.35"))
        .toRight("this certificate carries no Authority Key Identifier, so it has no ARI id")
      inner <- unwrap(raw, 0x04).map(_._1).toRight("the AKI extension is not an OCTET STRING")
      seq <- unwrap(inner, 0x30).map(_._1).toRight("the AKI does not hold a SEQUENCE")
      keyId <- unwrap(seq, 0x80.toByte).map(_._1).toRight("the AKI holds no keyIdentifier")
    yield b64(keyId) + "." + b64(leaf.getSerialNumber.toByteArray)

  /** one DER value of the expected tag: its content, and where it
   * ended. Short and long form lengths, nothing else -- this reads
   * two known shapes, it is not an ASN.1 library. */
  private def unwrap(der: Array[Byte], tag: Byte): Option[(Array[Byte], Int)] =
    if der.isEmpty || der(0) != tag || der.length < 2 then None
    else
      val first = der(1) & 0xff
      val (len, from) =
        if first < 0x80 then (first, 2)
        else
          val n = first & 0x7f
          if n == 0 || n > 4 || der.length < 2 + n then (-1, 0)
          else ((2 until 2 + n).foldLeft(0)((acc, i) => (acc << 8) | (der(i) & 0xff)), 2 + n)
      if len < 0 || from + len > der.length then None
      else Some((der.slice(from, from + len), from + len))

  private[acme] def leafCert(certFile: Path): Either[String, java.security.cert.X509Certificate] =
    try
      val cf = java.security.cert.CertificateFactory.getInstance("X.509")
      val in = Files.newInputStream(certFile)
      val certs = try cf.generateCertificates(in) finally in.close()
      if certs.isEmpty then Left(s"$certFile holds no certificate")
      else certs.iterator().next() match
        case x: java.security.cert.X509Certificate => Right(x)
        case other => Left(s"$certFile holds a ${other.getType} certificate, not X.509")
    catch case e: Exception => Left(s"$certFile is not a readable certificate: ${e.getMessage}")

  /** the whole flow, unconditionally */
  def issue(cfg: Config, http: Http, challenges: Challenges)
           (using Crypto, CanBlock): Either[String, Unit] =
    for
      _ <- Either.cond(cfg.domains.nonEmpty, (), "ACME needs at least one domain")
      account <- accountKeyOf(cfg.accountKey)
      dir <- get(http, cfg.directory).flatMap(json)
      newNonce <- str(dir, "newNonce").toRight("the directory has no newNonce")
      newAccount <- str(dir, "newAccount").toRight("the directory has no newAccount")
      newOrder <- str(dir, "newOrder").toRight("the directory has no newOrder")
      session = new Session(http, account, newNonce, cfg.eab)
      _ <- session.register(newAccount, cfg.email)
      order <- session.post(newOrder, Json.print(Json.JObj(Vector(
        "identifiers" -> Json.JArr(cfg.domains.map(d =>
          Json.JObj(Vector("type" -> Json.JStr("dns"), "value" -> Json.JStr(d)))))))))
      orderUrl <- header(order.head, "location").toRight("newOrder answered no Location")
      authorizations <- strings(order.body, "authorizations")
      finalizeUrl <- str(order.body, "finalize").toRight("the order has no finalize")
      _ <- authorizations.foldLeft[Either[String, Unit]](Right(())) { (acc, a) =>
        acc.flatMap(_ => authorize(session, a, challenges, cfg.timeout))
      }
      _ <- certificateKey(cfg.keyFile)
      csr <- csrOf(cfg.keyFile, cfg.domains)
      _ <- session.post(finalizeUrl, Json.print(Json.JObj(Vector("csr" -> Json.JStr(b64(csr))))))
      certUrl <- awaitOrder(session, orderUrl, cfg.timeout)
      pem <- session.postAsGetText(certUrl)
      _ <- write(cfg.certFile, pem)
    yield ()

  // ---- the steps -------------------------------------------------

  private def authorize(session: Session, url: String, challenges: Challenges,
                        timeout: java.time.Duration): Either[String, Unit] =
    for
      auth <- session.postAsGet(url)
      challenge <- httpChallenge(auth).toRight(s"no http-01 challenge in $url")
      (challengeUrl, token) = challenge
      keyAuth = token + "." + session.thumbprint
      _ = challenges.put(token, keyAuth)
      _ <- session.post(challengeUrl, "{}")
      _ <- await(timeout, s"authorization $url") {
        session.postAsGet(url).map(a => str(a, "status").getOrElse("pending")) match
          case Right("valid") => Some(Right(()))
          case Right("invalid") => Some(Left(s"the CA refused $url: ${detail(session, url)}"))
          case Right(_) => None
          case Left(m) => Some(Left(m))
      }
      _ = challenges.remove(token)
    yield ()

  private def detail(session: Session, url: String): String =
    session.postAsGet(url).map(Json.print).getOrElse("(no detail)").take(300)

  private def awaitOrder(session: Session, orderUrl: String, timeout: java.time.Duration)
  : Either[String, String] =
    await(timeout, s"order $orderUrl") {
      session.postAsGet(orderUrl) match
        case Left(m) => Some(Left(m))
        case Right(o) => str(o, "status") match
          case Some("valid") => Some(str(o, "certificate").toRight("a valid order with no certificate URL"))
          case Some("invalid") => Some(Left(s"the CA refused the order: ${Json.print(o).take(300)}"))
          case _ => None
    }

  /** poll until the step answers, or say which step ran out of time */
  private def await[A](timeout: java.time.Duration, what: String)(step: => Option[Either[String, A]])
  : Either[String, A] =
    val deadline = System.currentTimeMillis() + timeout.toMillis
    var answer: Option[Either[String, A]] = step
    while answer.isEmpty && System.currentTimeMillis() < deadline do
      Thread.sleep(500)
      answer = step
    answer.getOrElse(Left(s"$what did not settle in ${timeout.toSeconds}s"))

  private def httpChallenge(auth: Json): Option[(String, String)] =
    field(auth, "challenges") match
      case Some(Json.JArr(cs)) => cs.collectFirst {
        case c if str(c, "type").contains("http-01") =>
          (str(c, "url").getOrElse(""), str(c, "token").getOrElse(""))
      }.filter((u, t) => u.nonEmpty && t.nonEmpty)
      case _ => None

  // ---- the session: nonces, JWS, and the account it signs as ------

  private final class Session(http: Http, account: KeyPair, newNonce: String,
                              eab: Option[(String, String)] = None)(using c: Crypto):
    private var nonce: Option[String] = None
    private var kid: Option[String] = None

    val thumbprint: String = b64(c.sha256(canonicalJwk(account.getPublic).getBytes(UTF_8)))

    def register(newAccount: String, email: String): Either[String, String] =
      val binding = eab.map((kid, macKey) => externalBinding(newAccount, kid, macKey)).toVector
      post(newAccount, Json.print(Json.JObj(Vector(
        "termsOfServiceAgreed" -> Json.JBool(true),
        "contact" -> Json.JArr(Vector(Json.JStr(s"mailto:$email")))) ++
        binding.map("externalAccountBinding" -> _)))).flatMap { r =>
        header(r.head, "location").toRight("newAccount answered no Location").map { k =>
          kid = Some(k)
          k
        }
      }

    def post(url: String, payload: String): Either[String, Answer] = post(url, payload, retried = false)

    private def post(url: String, payload: String, retried: Boolean): Either[String, Answer] =
      val attempt =
        for
          n <- freshNonce()
          body <- sign(url, n, payload)
          r <- send(Request.post(url, Body.Text(body), Vector("Content-Type" -> "application/jose+json")))
        yield r
      attempt match
        // RFC 8555 §6.5: a badNonce answer carries a fresh nonce and
        // the client SHOULD retry once with it. Pebble rejects a
        // reused nonce where our own double did not -- which is how
        // this retry (and the bug under it) was found.
        case Left(msg) if !retried && msg.contains("badNonce") => post(url, payload, retried = true)
        case other => other

    /** POST-as-GET: an EMPTY payload, which is how RFC 8555 reads a
     * resource -- there is no GET with authentication in this protocol */
    def postAsGet(url: String): Either[String, Json] = post(url, "").map(_.body)

    def postAsGetText(url: String): Either[String, String] = post(url, "").map(_.text)

    /** a nonce is spent ONCE: taking it clears the cache, and asking
     * the CA for one leaves the cache empty too.
     *
     * The first version returned the `Replay-Nonce` of the HEAD it
     * had just made WITHOUT clearing what `send` had cached from that
     * same response — so the next POST spent the same value again and
     * a strict CA answered badNonce. Our own test double accepted it;
     * Pebble did not, which is the whole reason acme-pebble exists. */
    private def freshNonce(): Either[String, String] = take() match
      case Some(n) => Right(n)
      case None =>
        send(Request(Method.Head, newNonce)).flatMap { _ =>
          take().toRight("the CA sent no Replay-Nonce")
        }

    private def take(): Option[String] =
      val n = nonce
      nonce = None
      n

    private def send(r: Request): Either[String, Answer] =
      try
        val res = Async.run[Response, Pure](http.send(r)).runWith
        val text = Async.run[String, Pure](Http.text(res)).runWith
        header(res, "replay-nonce").foreach(n => nonce = Some(n))
        val parsed = if text.isEmpty then Json.JObj(Vector.empty) else Json.parse(text)
        if res.status >= 400 then Left(problem(res.status, text))
        else Right(Answer(res, parsed, text))
      catch case e: Throwable => Left(s"${r.method.name} ${r.url} failed: ${Option(e.getMessage).getOrElse(e.toString)}")

    /**
     * The INNER JWS a CA asks for when it will not open an account for
     * a stranger (RFC 8555 §7.3.4): our own account JWK as the
     * payload, signed HS256 with the MAC key the CA gave us out of
     * band, its protected header naming that key's id and the
     * newAccount URL. It proves the ACME account and the account we
     * already have with that CA are the same customer.
     *
     * No nonce here, deliberately: the inner JWS is not a request,
     * it is a credential carried inside one, and RFC 8555 says its
     * protected header has exactly `alg`, `kid` and `url`.
     */
    private def externalBinding(newAccount: String, kid: String, macKey: String): Json =
      val protectedHeader = Json.print(Json.JObj(Vector(
        "alg" -> Json.JStr("HS256"),
        "kid" -> Json.JStr(kid),
        "url" -> Json.JStr(newAccount))))
      val payload = canonicalJwk(account.getPublic)
      val signingInput = b64s(protectedHeader) + "." + b64s(payload)
      // the MAC key travels base64url, as every CA that issues one
      // writes it -- padded or not, both are accepted here because
      // half of them pad
      val key = java.util.Base64.getUrlDecoder.decode(macKey.replace("=", ""))
      val sig = c.hmacSha256(key, signingInput.getBytes(UTF_8))
      Json.JObj(Vector(
        "protected" -> Json.JStr(b64s(protectedHeader)),
        "payload" -> Json.JStr(b64s(payload)),
        "signature" -> Json.JStr(b64(sig))))

    private def sign(url: String, n: String, payload: String): Either[String, String] =
      val protectedHeader = Json.JObj(Vector(
        "alg" -> Json.JStr("ES256"),
        "nonce" -> Json.JStr(n),
        "url" -> Json.JStr(url)) ++
        // the FIRST request signs with the key itself; every one after
        // it with the account URL the CA answered (RFC 8555 §6.2)
        Vector(kid.map(k => "kid" -> Json.JStr(k))
          .getOrElse("jwk" -> Json.parse(canonicalJwk(account.getPublic)))))
      val signingInput = b64s(Json.print(protectedHeader)) + "." + b64s(payload)
      val der = summon[Crypto].signEcdsaSha256(Crypto.Handle(account.getPrivate), signingInput.getBytes(UTF_8))
      Es256.derToJose(der).toRight("the platform's ECDSA signature did not parse").map { raw =>
        Json.print(Json.JObj(Vector(
          "protected" -> Json.JStr(b64s(Json.print(protectedHeader))),
          "payload" -> Json.JStr(b64s(payload)),
          "signature" -> Json.JStr(b64(raw)))))
      }

  private final case class Answer(head: Response, body: Json, text: String)

  /** the CA's own error shape (application/problem+json), read as the
   * sentence an operator needs rather than a status code */
  private def problem(status: Int, text: String): String =
    val j = try Json.parse(text) catch case _: Throwable => Json.JNull
    val kind = str(j, "type").getOrElse("")
    val detail = str(j, "detail").getOrElse(text.take(300))
    s"the CA answered $status: $detail" + (if kind.isEmpty then "" else s" ($kind)")

  // ---- keys, JWK, CSR --------------------------------------------

  /** RFC 7638: the members in lexicographic order, no whitespace --
   * the thumbprint is a hash of THIS string, so its shape is the
   * specification and not a formatting choice */
  private[acme] def canonicalJwk(pub: PublicKey): String =
    val ec = pub match
      case k: ECPublicKey => k
      case other => throw IllegalArgumentException(s"an ACME account key is EC P-256, got ${other.getAlgorithm}")
    s"""{"crv":"P-256","kty":"EC","x":"${b64(coord(ec.getW.getAffineX))}","y":"${b64(coord(ec.getW.getAffineY))}"}"""

  /** a P-256 coordinate is 32 bytes, big-endian, left-padded */
  private def coord(v: java.math.BigInteger): Array[Byte] =
    val raw = v.toByteArray.dropWhile(_ == 0)
    Array.fill[Byte](32 - raw.length)(0) ++ raw

  /** the account key: read when it is there, generated and written
   * when it is not -- an account key that changes is a new account */
  private[acme] def accountKeyOf(path: Path): Either[String, KeyPair] =
    try
      val pub = path.resolveSibling(path.getFileName.toString + ".pub")
      if Files.isRegularFile(path) && Files.isRegularFile(pub) then
        val kf = java.security.KeyFactory.getInstance("EC")
        Right(KeyPair(
          kf.generatePublic(X509EncodedKeySpec(der(Files.readString(pub)))),
          kf.generatePrivate(PKCS8EncodedKeySpec(der(Files.readString(path))))))
      else
        val pair = generateEc()
        Option(path.getParent).foreach(Files.createDirectories(_))
        for
          _ <- write(path, pem("PRIVATE KEY", pair.getPrivate.getEncoded))
          _ <- write(pub, pem("PUBLIC KEY", pair.getPublic.getEncoded))
        yield pair
    catch case e: Exception => Left(s"the ACME account key at $path is not usable: ${e.getMessage}")

  /** the certificate's own key, generated once and kept: a renewal
   * reuses it, which is what makes a pinned key survive one */
  private[acme] def certificateKey(path: Path): Either[String, Unit] =
    try
      if Files.isRegularFile(path) then Right(())
      else
        Option(path.getParent).foreach(Files.createDirectories(_))
        write(path, pem("PRIVATE KEY", generateEc().getPrivate.getEncoded))
    catch case e: Exception => Left(s"the certificate key at $path could not be written: ${e.getMessage}")

  private def generateEc(): KeyPair =
    val g = KeyPairGenerator.getInstance("EC")
    g.initialize(ECGenParameterSpec("secp256r1"))
    g.generateKeyPair()

  /**
   * The PKCS#10 the CA signs. openssl, and a NAMED refusal without
   * it: no exported JDK API builds a certificate request
   * (`sun.security.pkcs10` is not open), and a crypto library added
   * for this one DER would be a dependency the stack does not carry.
   */
  private[acme] def csrOf(keyFile: Path, domains: Vector[String]): Either[String, Array[Byte]] =
    val out = Files.createTempFile("okay-acme-", ".der")
    try
      val san = domains.map(d => s"DNS:$d").mkString(",")
      val cmd = Vector("openssl", "req", "-new", "-key", keyFile.toString,
        "-subj", s"/CN=${domains.head}", "-addext", s"subjectAltName=$san",
        "-outform", "DER", "-out", out.toString)
      val p = ProcessBuilder(cmd*).redirectErrorStream(true).start()
      val log = String(p.getInputStream.readAllBytes(), UTF_8)
      if p.waitFor() != 0 then
        Left(s"openssl could not build the certificate request: ${log.trim.take(300)}")
      else Right(Files.readAllBytes(out))
    catch
      case _: java.io.IOException =>
        Left("ACME needs openssl on the PATH to build the certificate request — " +
          "no exported JDK API builds a PKCS#10; a proxy that speaks ACME is the other road")
      case e: Exception => Left(s"the certificate request failed: ${e.getMessage}")
    finally Files.deleteIfExists(out): Unit

  /** when does the certificate on disk stop being valid? `None` when
   * there is no readable certificate there yet -- which is how a
   * caller asks "has this ever been issued?" */
  def notAfterOf(certFile: Path): Option[java.time.Instant] =
    try
      if !Files.isRegularFile(certFile) then None
      else
        val cf = java.security.cert.CertificateFactory.getInstance("X.509")
        val in = Files.newInputStream(certFile)
        val certs = try cf.generateCertificates(in) finally in.close()
        certs.iterator().next() match
          case x: java.security.cert.X509Certificate => Some(x.getNotAfter.toInstant)
          case _ => None
    catch case _: Exception => None

  // ---- small shared pieces ---------------------------------------

  private val encoder = java.util.Base64.getUrlEncoder.withoutPadding
  private def b64(bytes: Array[Byte]): String = encoder.encodeToString(bytes)
  private def b64s(s: String): String = b64(s.getBytes(UTF_8))
  private def der(pem: String): Array[Byte] =
    java.util.Base64.getMimeDecoder.decode(pem.linesIterator.filterNot(_.startsWith("-----")).mkString)
  private def pem(kind: String, der: Array[Byte]): String =
    val body = java.util.Base64.getMimeEncoder(64, Array[Byte]('\n')).encodeToString(der)
    s"-----BEGIN $kind-----\n$body\n-----END $kind-----\n"
  private def write(p: Path, content: String): Either[String, Unit] =
    try
      Option(p.getParent).foreach(Files.createDirectories(_))
      Files.writeString(p, content): Unit
      Right(())
    catch case e: Exception => Left(s"$p could not be written: ${e.getMessage}")

  private def get(http: Http, url: String)(using CanBlock): Either[String, Response] =
    try Right(Async.run[Response, Pure](http.send(Request.get(url))).runWith)
    catch case e: Throwable => Left(s"GET $url failed: ${Option(e.getMessage).getOrElse(e.toString)}")

  private def json(r: Response)(using CanBlock): Either[String, Json] =
    try Right(Json.parse(Async.run[String, Pure](Http.text(r)).runWith))
    catch case e: Throwable => Left(s"the CA's answer is not JSON: ${Option(e.getMessage).getOrElse(e.toString)}")

  private def header(r: Response, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  private[acme] def field(j: Json, name: String): Option[Json] = j match
    case Json.JObj(fs) => fs.collectFirst { case (k, v) if k == name => v }
    case _ => None
  private[acme] def str(j: Json, name: String): Option[String] =
    field(j, name).collect { case Json.JStr(s) => s }
  private def strings(j: Json, name: String): Either[String, Vector[String]] =
    field(j, name) match
      case Some(Json.JArr(vs)) => Right(vs.collect { case Json.JStr(s) => s })
      case _ => Left(s"the order has no $name")
