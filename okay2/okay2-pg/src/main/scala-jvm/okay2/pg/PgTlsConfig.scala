package okay2.pg

import java.net.Socket
import java.security.KeyStore
import java.security.cert.{CertificateFactory, X509Certificate}
import javax.net.ssl._

/**
 * The TLS client the pg sslmode connect needs (okay-tls's Tls.client,
 * specs/tls.md), on the JVM's own `SSLSocket`. okay-tls itself is not
 * ported to okay2: this is its CLIENT half only — the sslmode ladder,
 * trust from a CA file or the platform store, and an mTLS identity —
 * kept inside okay2-pg until a second okay2 wire wants TLS.
 *
 * One difference, stated: okay-tls takes the client key as a `Secret`
 * reference resolved through okay-conf, which okay2 does not have; here
 * `clientKey` is the PATH of an unencrypted PKCS#8 PEM file (0400), read
 * at connect — a reference still, never key material in the config, and
 * a value that holds PEM is refused by name as okay-tls refuses it.
 */
sealed trait SslMode

object SslMode {
  /** plaintext — connects, loggable as the named decision it is */
  case object Disable extends SslMode
  /** encrypt, no identity check: a tunnel, not authentication */
  case object Require extends SslMode
  /** the chain checks out; the HOSTNAME IS NOT CHECKED */
  case object VerifyCa extends SslMode
  /** chain and hostname — the default, the only honest one */
  case object VerifyFull extends SslMode
}

final case class TlsConfig(mode: SslMode = SslMode.VerifyFull,
                           caFile: Option[String] = None,
                           clientCert: Option[String] = None,
                           clientKey: Option[String] = None)

object Tls {

  /** wrap an already-connected client socket BEFORE any protocol bytes
   * flow; `host` is what the certificate must name under VerifyFull.
   * Refusals are values naming what failed */
  def client(sock: Socket, host: String, cfg: TlsConfig = TlsConfig()): Either[String, Socket] =
    cfg.mode match {
      case SslMode.Disable => Right(sock)
      case mode =>
        for {
          _ <- noInlineKey(cfg.clientKey)
          identity <- clientIdentity(cfg)
          trust <- trustOf(mode, cfg.caFile)
          ctx <- contextOf(trust, identity)
          out <- handshake(ctx, sock, host, mode)
        } yield out
    }

  /** the key must BE a reference — PEM in it is the leak a reference
   * exists to prevent */
  private def noInlineKey(key: Option[String]): Either[String, Unit] = key match {
    case Some(s) if s.contains("-----BEGIN") =>
      Left("the private key is INLINE in the config — a key travels as a file reference, never as material")
    case _ => Right(())
  }

  /** mTLS: cert AND key make an identity; one without the other is a
   * misconfiguration named as such */
  private def clientIdentity(cfg: TlsConfig): Either[String, Option[(String, String)]] =
    (cfg.clientCert, cfg.clientKey) match {
      case (None, None) => Right(None)
      case (Some(cert), Some(key)) =>
        try Right(Some((cert, new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(key)), "UTF-8"))))
        catch { case e: Exception => Left(s"client key '$key' did not read: ${e.getMessage}") }
      case (Some(_), None) => Left("clientCert is set without clientKey — a client identity is a certificate AND its key")
      case (None, Some(_)) => Left("clientKey is set without clientCert — a client identity is a certificate AND its key")
    }

  private def trustOf(mode: SslMode, caFile: Option[String]): Either[String, Option[Array[TrustManager]]] =
    mode match {
      case SslMode.Require =>
        // encrypt-only: trust anything, check nothing — the tunnel mode,
        // opted into BY NAME
        Right(Some(Array[TrustManager](new X509TrustManager {
          def checkClientTrusted(c: Array[X509Certificate], a: String): Unit = ()
          def checkServerTrusted(c: Array[X509Certificate], a: String): Unit = ()
          def getAcceptedIssuers: Array[X509Certificate] = Array.empty
        })))
      case _ => caFile match {
        case None => Right(None) // the platform's CA store
        case Some(path) =>
          try {
            val cf = CertificateFactory.getInstance("X.509")
            val in = java.nio.file.Files.newInputStream(java.nio.file.Paths.get(path))
            val certs = try cf.generateCertificates(in) finally in.close()
            val ks = KeyStore.getInstance(KeyStore.getDefaultType)
            ks.load(null, null)
            val it = certs.iterator; var i = 0
            while (it.hasNext) { ks.setCertificateEntry(s"ca$i", it.next); i += 1 }
            val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
            tmf.init(ks)
            Right(Some(tmf.getTrustManagers))
          } catch { case e: Exception => Left(s"CA file '$path' did not load: ${e.getMessage}") }
      }
    }

  /** `identity` is (PEM chain path, PKCS#8 key PEM) */
  private def contextOf(trust: Option[Array[TrustManager]], identity: Option[(String, String)]): Either[String, SSLContext] =
    try {
      val kms = identity match {
        case None => null
        case Some((certFile, keyPem)) =>
          val cf = CertificateFactory.getInstance("X.509")
          val in = java.nio.file.Files.newInputStream(java.nio.file.Paths.get(certFile))
          val chain = try cf.generateCertificates(in).toArray(Array.empty[java.security.cert.Certificate]) finally in.close()
          val key = privateKey(keyPem).fold(m => throw new IllegalStateException(m), k => k)
          val ks = KeyStore.getInstance(KeyStore.getDefaultType)
          ks.load(null, null)
          ks.setKeyEntry("identity", key, Array.empty[Char], chain)
          val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
          kmf.init(ks, Array.empty[Char])
          kmf.getKeyManagers
      }
      val ctx = SSLContext.getInstance("TLS")
      ctx.init(kms, trust.orNull, null)
      Right(ctx)
    } catch { case e: Exception => Left(s"TLS context did not build: ${e.getMessage}") }

  /** a PKCS#8 PEM private key, whatever its algorithm; an encrypted or a
   * PKCS#1/SEC1 key is refused by name */
  def privateKey(pem: String): Either[String, java.security.PrivateKey] = {
    val head = pem.linesIterator.find(_.startsWith("-----BEGIN")).map(_.trim).getOrElse("")
    if (head.contains("ENCRYPTED"))
      Left("the private key is encrypted; this seam has no passphrase to give it — " +
        "supply an unencrypted PKCS#8 key (what certbot writes)")
    else if (head.contains("RSA PRIVATE KEY") || head.contains("EC PRIVATE KEY"))
      Left(s"'$head' is a PKCS#1/SEC1 key, not PKCS#8 — convert it once: " +
        "openssl pkcs8 -topk8 -nocrypt -in key.pem -out key.pk8.pem")
    else {
      val body = pem.linesIterator.filterNot(_.startsWith("-----")).mkString.replaceAll("\\s", "")
      try {
        val spec = new java.security.spec.PKCS8EncodedKeySpec(java.util.Base64.getDecoder.decode(body))
        val tried = Vector("RSA", "EC", "Ed25519", "DSA")
        tried.iterator.map { a =>
          try Right(java.security.KeyFactory.getInstance(a).generatePrivate(spec))
          catch { case _: Exception => Left(a) }
        }.collectFirst { case Right(k) => k }
          .toRight(s"the private key is none of ${tried.mkString(", ")} — an unencrypted PKCS#8 PEM is what this seam reads")
      } catch { case e: Exception => Left(s"the private key is not readable PEM: ${e.getMessage}") }
    }
  }

  private def handshake(ctx: SSLContext, sock: Socket, host: String, mode: SslMode): Either[String, Socket] =
    try {
      val ssl = ctx.getSocketFactory.createSocket(sock, host, sock.getPort, true) match {
        case s: SSLSocket => s
        case other => throw new IllegalStateException(s"the SSL factory answered a plain socket: $other")
      }
      ssl.setUseClientMode(true)
      if (mode == SslMode.VerifyFull) {
        val p = ssl.getSSLParameters
        // the hostname check — exactly what VerifyCa does NOT do
        p.setEndpointIdentificationAlgorithm("HTTPS")
        ssl.setSSLParameters(p)
      }
      ssl.startHandshake()
      Right(ssl)
    } catch {
      case e: SSLHandshakeException => Left(s"TLS handshake with '$host' refused (${modeName(mode)}): ${e.getMessage}")
      case e: Exception => Left(s"TLS with '$host' failed: ${e.getMessage}")
    }

  private[pg] def modeName(m: SslMode): String = m.toString.toLowerCase
}
