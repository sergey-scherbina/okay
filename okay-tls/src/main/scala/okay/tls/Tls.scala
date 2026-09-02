package okay.tls

import okay.conf.{Secret, Secrets}
import java.net.Socket
import java.security.KeyStore
import java.security.cert.{CertificateFactory, X509Certificate}
import javax.net.ssl.*

/**
 * TLS for the own wires (specs/tls.md): encryption wraps the
 * TRANSPORT, not the protocols — every wire above gets TLS from this
 * one seam and adds nothing of its own. The vocabulary is postgres's
 * `sslmode`, adopted stack-wide because operators already know it
 * and it names the honest levels; `VerifyFull` is the only default,
 * anything weaker is a decision with a name.
 *
 * Platform crypto only (the security doctrine extended to
 * transport): the JVM leg is `SSLSocket` — the platform's TLS for
 * the blocking sockets our wires actually run on, a virtual thread
 * parking in the handshake exactly as it parks in the read.
 */
enum SslMode:
  case Disable      // plaintext — connects, loggable as the named decision it is
  case Require      // encrypt, no identity check: a tunnel, not authentication
  case VerifyCa     // the chain checks out; the HOSTNAME IS NOT CHECKED
  case VerifyFull   // chain and hostname — the default, the only honest one

/**
 * cert/CA paths are plain fields; the PRIVATE KEY is a Secret
 * reference (`file:` under 0400) — a key never inlines, and a ref
 * that smuggles PEM inline is refused at the seam.
 */
final case class TlsConfig(mode: SslMode = SslMode.VerifyFull,
                           caFile: Option[String] = None,
                           clientCert: Option[String] = None,   // mTLS: the identity the CLIENT presents (cert PEM path) …
                           clientKey: Option[Secret] = None)    // … and its key, a ref like the server's

object Tls {

  /**
   * Wrap an already-connected client socket BEFORE any protocol
   * bytes flow. `host` is what the certificate must name under
   * VerifyFull. Refusals are values naming what failed.
   */
  def client(sock: Socket, host: String, cfg: TlsConfig = TlsConfig(),
             secrets: Secrets = Secrets.env): Either[String, Socket] =
    cfg.mode match
      case SslMode.Disable => Right(sock)
      case mode =>
        for
          _ <- noInlineKey(cfg.clientKey)
          identity <- clientIdentity(cfg, secrets)
          trust <- trustOf(mode, cfg.caFile)
          ctx <- contextOf(trust, identity)
          out <- handshake(ctx, sock, host, mode)
        yield out

  /** mTLS (pg-mtls): cert AND key make an identity the handshake
   * offers when the server asks; one without the other is a
   * misconfiguration named as such, never a silent no-identity */
  private def clientIdentity(cfg: TlsConfig, secrets: Secrets)
  : Either[String, Option[(String, String)]] = (cfg.clientCert, cfg.clientKey) match
    case (None, None) => Right(None)
    case (Some(cert), Some(key)) => secrets.get(key).map(pem => Some((cert, pem)))
    case (Some(_), None) => Left("clientCert is set without clientKey — a client identity is a certificate AND its key")
    case (None, Some(_)) => Left("clientKey is set without clientCert — a client identity is a certificate AND its key")

  /** the server half: a wire server terminates TLS itself — cert as
   * a PEM path, key as a Secret ref (PKCS#8 PEM) */
  def serverSocket(port: Int, certFile: String, key: Secret,
                   secrets: Secrets): Either[String, SSLServerSocket] =
    for
      _ <- noInlineKey(Some(key))
      pem <- secrets.get(key)
      ctx <- contextOf(None, Some((certFile, pem)))
    yield
      val ss = ctx.getServerSocketFactory.createServerSocket(port)
        .asInstanceOf[SSLServerSocket]
      ss

  /** the ambient-Secrets door (ctx-everywhere), wiring-shaped: the
   * server awaiting its resolver — provide(secrets) { Tls.served(...) } */
  def served(port: Int, certFile: String, key: Secret)
  : Secrets ?=> Either[String, SSLServerSocket] =
    serverSocket(port, certFile, key, summon[Secrets])

  // ── the pieces ────────────────────────────────────────────────────

  /** the ref must BE a reference — PEM in the ref is the leak the
   * Secret type exists to prevent */
  private def noInlineKey(key: Option[Secret]): Either[String, Unit] = key match
    case Some(s) if s.ref.contains("-----BEGIN") =>
      Left("the private key is INLINE in the config — a key travels as a Secret reference (file:/run/secrets/..), never as material")
    case _ => Right(())

  private def trustOf(mode: SslMode, caFile: Option[String])
  : Either[String, Option[Array[TrustManager]]] = mode match
    case SslMode.Require =>
      // encrypt-only: trust anything, check nothing — the tunnel mode,
      // opted into BY NAME
      Right(Some(Array(new X509TrustManager:
        def checkClientTrusted(c: Array[X509Certificate], a: String): Unit = ()
        def checkServerTrusted(c: Array[X509Certificate], a: String): Unit = ()
        def getAcceptedIssuers: Array[X509Certificate] = Array.empty)))
    case _ => caFile match
      case None => Right(None)   // the platform's CA store
      case Some(path) =>
        try
          val cf = CertificateFactory.getInstance("X.509")
          val in = java.nio.file.Files.newInputStream(java.nio.file.Paths.get(path))
          val certs = try cf.generateCertificates(in) finally in.close()
          val ks = KeyStore.getInstance(KeyStore.getDefaultType)
          ks.load(null, null)
          val it = certs.iterator; var i = 0
          while it.hasNext do { ks.setCertificateEntry(s"ca$i", it.next); i += 1 }
          val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
          tmf.init(ks)
          Right(Some(tmf.getTrustManagers))
        catch case e: Exception => Left(s"CA file '$path' did not load: ${e.getMessage}")

  /** `identity` is (PEM chain path, PKCS#8 key PEM) — the server's
   * own on the server half, the client's on an mTLS client */
  private def contextOf(trust: Option[Array[TrustManager]],
                        identity: Option[(String, String)])
  : Either[String, SSLContext] =
    try
      val kms = identity match
        case None => null
        case Some((certFile, keyPem)) =>
          val cf = CertificateFactory.getInstance("X.509")
          val in = java.nio.file.Files.newInputStream(java.nio.file.Paths.get(certFile))
          val chain = try cf.generateCertificates(in).toArray(Array.empty[java.security.cert.Certificate]) finally in.close()
          val body = keyPem.linesIterator.filterNot(_.startsWith("-----")).mkString
          val der = java.util.Base64.getDecoder.decode(body)
          val key = java.security.KeyFactory.getInstance("RSA")
            .generatePrivate(java.security.spec.PKCS8EncodedKeySpec(der))
          val ks = KeyStore.getInstance(KeyStore.getDefaultType)
          ks.load(null, null)
          ks.setKeyEntry("identity", key, Array.empty, chain)
          val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
          kmf.init(ks, Array.empty)
          kmf.getKeyManagers
      val ctx = SSLContext.getInstance("TLS")
      ctx.init(kms, trust.orNull, null)
      Right(ctx)
    catch case e: Exception => Left(s"TLS context did not build: ${e.getMessage}")

  private def handshake(ctx: SSLContext, sock: Socket, host: String,
                        mode: SslMode): Either[String, Socket] =
    try
      val ssl = ctx.getSocketFactory
        .createSocket(sock, host, sock.getPort, true).asInstanceOf[SSLSocket]
      ssl.setUseClientMode(true)
      if mode == SslMode.VerifyFull then
        val p = ssl.getSSLParameters
        // the hostname check — exactly what VerifyCa does NOT do
        p.setEndpointIdentificationAlgorithm("HTTPS")
        ssl.setSSLParameters(p)
      ssl.startHandshake()
      Right(ssl)
    catch
      case e: SSLHandshakeException =>
        Left(s"TLS handshake with '$host' refused (${mode.toString.toLowerCase}): ${e.getMessage}")
      case e: Exception =>
        Left(s"TLS with '$host' failed: ${e.getMessage}")
}
