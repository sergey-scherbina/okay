package okay.script

import okay.*
import okay.given
import okay.persist.FileStore
import okay.security.given

import java.nio.file.{Files, Path, Paths}

/** The stock entry point: a directory of pages, served -- no code of
 * the caller's own. See specs/okay-script.md "Serving".
 *
 *   sbt "okayScript/runMain okay.script.Serve pages 8080"
 *   OKAY_DATA=./data sbt "okayScript/runMain okay.script.Serve pages"
 *
 * `OKAY_ACME=<email>` with `OKAY_ACME_DOMAINS=a,b` asks a certificate
 * authority for the certificate itself (staging unless
 * `OKAY_ACME_PROD=1`); the challenge is served on `OKAY_HTTP_PORT`,
 * which must be reachable from the internet on port 80.
 *
 * `OKAY_ACME_EAB=<kid>:<key>` carries the external account binding a
 * commercial CA requires (Let's Encrypt does not).
 *
 * `OKAY_TLS_RELOAD=<seconds>` re-reads the certificate when it
 * changes on disk, so certbot's renewal needs no restart.
 *
 * `OKAY_OPS=1` mounts /healthz, /stats and /metrics beside the pages.
 * `OKAY_FORWARDED=1` trusts `X-Forwarded-Proto`, so a Site behind a
 * TLS-terminating proxy still marks its cookies `Secure`.
 *
 * `OKAY_LANGS=en,uk` names the languages the site speaks (okay-script-
 * i18n), the first the default. `OKAY_TLS_CERT=/path/cert.pem` with
 * `OKAY_TLS_KEY=file:/path/key.pem` (a Secret ref) serves HTTPS
 * through the one transport seam (script-tls).
 *
 * `OKAY_DATA` names a directory for an okay-persist `FileStore`: with
 * it the sessions (`Sessions.persisted`) and the application scope
 * (`Application.persisted`) survive a restart; without it both are in
 * memory. Runs until interrupted (Ctrl-C, a SIGTERM): the `Resource`
 * releases and the server stops -- the lifecycle proof, applied.
 */
object Serve:

  final case class Args(root: Path, port: Int, data: Option[Path], languages: Vector[String] = Vector("en"),
                        tls: Option[(String, okay.conf.Secret)] = None,
                        /** OKAY_OPS=1 mounts /healthz, /stats and /metrics
                         * beside the pages -- opt-in, because exposure is
                         * the deployment's decision, not a directory's */
                        ops: Boolean = false,
                        /** OKAY_FORWARDED=1: trust `X-Forwarded-Proto` --
                         * for a Site behind a proxy YOU control, which is
                         * the only place a header is evidence */
                        forwarded: Boolean = false,
                        /** OKAY_TLS=self: a self-signed certificate,
                         * generated once into the data directory (or a
                         * temp one) and reused -- https with nothing to
                         * obtain first (script-https-default) */
                        selfSigned: Boolean = false,
                        /** OKAY_HSTS=<seconds> */
                        hsts: Option[Int] = None,
                        /** OKAY_HTTPS_ONLY=1 */
                        httpsOnly: Boolean = false,
                        /** OKAY_HTTP_PORT=<n>: a plaintext port that only
                         * redirects to https, for a deployment with no
                         * proxy in front */
                        httpPort: Option[Int] = None,
                        /** OKAY_TLS_RELOAD=<seconds>: re-read the
                         * certificate when it changes, so a renewal is
                         * picked up without a restart (script-real-certs) */
                        tlsReload: Option[Int] = None,
                        /** OKAY_ACME=<email> with OKAY_ACME_DOMAINS=a,b:
                         * ask a certificate authority for the certificate
                         * (okay-acme). Staging unless OKAY_ACME_PROD=1 */
                        acme: Option[(String, Vector[String], Boolean)] = None,
                        /** OKAY_ACME_EAB=<kid>:<base64url MAC key>: the
                         * external account binding a commercial CA hands
                         * you out of band (acme-eab) */
                        acmeEab: Option[(String, String)] = None):
    def scheme: String = if tls.isDefined then "https" else "http"

  /** `<dir> [port]`; port 8080 by default. With NO arguments the
   * environment answers instead -- `OKAY_PAGES` and `OKAY_PORT` --
   * because a container's entrypoint is `java -jar app.jar` and the
   * pages directory rides in as configuration, not as a command line
   * (okay-script-image). */
  def parse(args: Array[String], env: String => Option[String] = k => Option(System.getenv(k))): Either[String, Args] =
    val words = if args.nonEmpty then args.toList
      else env("OKAY_PAGES").toList ++ env("OKAY_PORT").toList
    words match
      case dir :: rest if rest.length <= 1 =>
        val root = Paths.get(dir)
        if !Files.isDirectory(root) then Left(s"not a directory: $dir")
        else
          rest.headOption.map(_.toIntOption.toRight(s"not a port: ${rest.head}")).getOrElse(Right(8080))
            .flatMap { port =>
              tlsOf(env).map(tls => Args(root, port, env("OKAY_DATA").map(Paths.get(_)),
                env("OKAY_LANGS").map(_.split(",").toVector.map(_.trim).filter(_.nonEmpty)).filter(_.nonEmpty).getOrElse(Vector("en")),
                tls,
                env("OKAY_OPS").exists(v => v == "1" || v.equalsIgnoreCase("true")),
                env("OKAY_FORWARDED").exists(v => v == "1" || v.equalsIgnoreCase("true")),
                env("OKAY_TLS").exists(_.equalsIgnoreCase("self")),
                env("OKAY_HSTS").flatMap(_.toIntOption).filter(_ > 0),
                env("OKAY_HTTPS_ONLY").exists(v => v == "1" || v.equalsIgnoreCase("true")),
                env("OKAY_HTTP_PORT").flatMap(_.toIntOption),
                env("OKAY_TLS_RELOAD").flatMap(_.toIntOption).filter(_ > 0),
                for
                  email <- env("OKAY_ACME")
                  domains = env("OKAY_ACME_DOMAINS").map(_.split(",").toVector.map(_.trim).filter(_.nonEmpty)).getOrElse(Vector.empty)
                  if domains.nonEmpty
                yield (email, domains, env("OKAY_ACME_PROD").exists(v => v == "1" || v.equalsIgnoreCase("true"))),
                env("OKAY_ACME_EAB").flatMap { pair =>
                  // kid:key -- the key is base64url and carries no colon,
                  // so the FIRST colon separates them
                  val i = pair.indexOf(':')
                  Option.when(i > 0 && i < pair.length - 1)((pair.take(i), pair.drop(i + 1)))
                }))
            }
      case _ => Left("usage: okay.script.Serve <pages-dir> [port]   (or OKAY_PAGES/OKAY_PORT; OKAY_DATA=<dir> for a persistent store)")

  /** OKAY_TLS_CERT and OKAY_TLS_KEY come as a PAIR: a certificate
   * without its key (or the other way round) is a misconfiguration
   * named as such, never a silent fall back to plaintext */
  private def tlsOf(env: String => Option[String]): Either[String, Option[(String, okay.conf.Secret)]] =
    (env("OKAY_TLS_CERT"), env("OKAY_TLS_KEY")) match
      case (None, None) => Right(None)
      case (Some(cert), Some(key)) => Right(Some((cert, okay.conf.Secret(key))))
      case (Some(_), None) => Left("OKAY_TLS_CERT is set without OKAY_TLS_KEY (a Secret ref: file:/run/secrets/key.pem)")
      case (None, Some(_)) => Left("OKAY_TLS_KEY is set without OKAY_TLS_CERT (the certificate PEM's path)")

  /** the TLS context the arguments describe, through the one
   * transport seam (specs/tls.md); a refusal names what failed */
  def sslOf(a: Args, secrets: okay.conf.Secrets = okay.conf.Secrets.chain(okay.conf.Secrets.env, okay.conf.Secrets.file))
  : Either[String, Option[javax.net.ssl.SSLContext]] =
    a.tls match
      case Some((cert, key)) => a.tlsReload match
        case None => okay.tls.Tls.serverContext(cert, key, secrets).map(Some(_))
        case Some(seconds) =>
          okay.tls.Tls.reloading(cert, key, secrets, java.time.Duration.ofSeconds(seconds),
            msg => System.err.println(s"okay-script: $msg")).map(Some(_))
      case None if a.acme.isDefined =>
        // the certificate is EARNED, then read like any other: ACME
        // writes the pair, and the reloading context picks up every
        // renewal after this one without a restart (okay-acme)
        val (files, _) = acmeFiles(a)
        okay.acme.Acme.notAfterOf(files._1) match
          case None => Left("OKAY_ACME is set but no certificate has been issued yet — " +
            "the run that issues it needs the challenge port reachable; see the log above")
          case Some(_) =>
            okay.tls.Tls.reloading(files._1.toString, okay.conf.Secret(s"file:${files._2}"), secrets,
              java.time.Duration.ofMinutes(5), msg => System.err.println(s"okay-script: $msg")).map(Some(_))
      case None if a.selfSigned =>
        // beside the data when there is a data directory, so a restart
        // keeps the same identity; a temp one otherwise, and then the
        // fingerprint changes with the machine's temp dir -- said, not
        // hidden, in the line it prints
        val ks = a.data.getOrElse(Paths.get(System.getProperty("java.io.tmpdir"))).resolve("okay-script-tls.p12")
        okay.tls.Tls.selfSigned(ks).map { (ctx, fp) =>
          println(s"okay-script: self-signed certificate in $ks")
          println(s"okay-script: its SHA-256 is $fp -- a browser will warn, because nobody vouched for it")
          Some(ctx)
        }
      case None => Right(None)

  /** where an ACME-issued identity lives: beside the data when there
   * is a data directory (so a restart keeps the account and the
   * certificate), in a temp directory otherwise -- which for ACME is
   * a bad idea and the caller is told so */
  def acmeFiles(a: Args): ((java.nio.file.Path, java.nio.file.Path), java.nio.file.Path) =
    val dir = a.data.getOrElse(Paths.get(System.getProperty("java.io.tmpdir"))).resolve("acme")
    ((dir.resolve("cert.pem"), dir.resolve("key.pem")), dir.resolve("account.pem"))

  /** run the ACME flow if it is configured: the challenge is served on
   * the PLAINTEXT port (a certificate authority speaks http and
   * follows no redirect for it), so `OKAY_HTTP_PORT` is required and
   * the refusal says so rather than hanging on a poll */
  def acmeRun(a: Args, challenges: okay.acme.Acme.Challenges)
             (using okay.security.Crypto, CanBlock): Either[String, Option[okay.acme.Acme.Outcome]] =
    a.acme match
      case None => Right(None)
      case Some((email, domains, prod)) =>
        val ((cert, key), account) = acmeFiles(a)
        if a.data.isEmpty then
          println("okay-script: OKAY_ACME without OKAY_DATA keeps the account key in a temp directory — " +
            "a restart then registers a NEW account, which a CA rate-limits")
        val cfg = okay.acme.Acme.Config(email, domains, account, cert, key,
          directory = if prod then okay.acme.Acme.Directory.letsEncrypt else okay.acme.Acme.Directory.letsEncryptStaging,
          eab = a.acmeEab)
        Resource.run[Either[String, okay.acme.Acme.Outcome], Pure](
          okay.jetty.Jetty.http().map(http => okay.acme.Acme.ensure(cfg, http, challenges))).runWith
          .map(Some(_))

  /** the Site the arguments describe -- a caller wanting `verify`/
   * `issue` or a shared `Sessions` builds its own from here */
  def site(a: Args): Site =
    a.data match
      case None => Site(a.root, languages = a.languages, trustForwarded = a.forwarded,
        hsts = a.hsts, httpsOnly = a.httpsOnly)
      case Some(dir) =>
        Files.createDirectories(dir)
        val store = FileStore.open(dir)
        Site(a.root, sessions = Sessions.persisted(store), application = api.Application.persisted(store),
          languages = a.languages, trustForwarded = a.forwarded, hsts = a.hsts, httpsOnly = a.httpsOnly)

  /** a whole server whose only answer is "the same URL, on https" --
   * the port `OKAY_HTTP_PORT` names when the site itself is TLS. The
   * authority comes from the request's own Host, with the TLS port
   * appended when it is not the standard one, because a redirect to
   * the wrong port is a redirect to nothing. */
  def redirectTo(tlsPort: Int): PartialFunction[okay.http.Request, okay.http.Response ! Async] = {
    case r =>
      val host = r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("host") => v }
        .map(_.takeWhile(_ != ':')).getOrElse("localhost")
      val authority = if tlsPort == 443 then host else s"$host:$tlsPort"
      pure(okay.http.Response(301, Vector("Location" -> s"https://$authority${r.url}"),
        okay.http.Http.one(Array.emptyByteArray)))
  }

  def main(args: Array[String]): Unit =
    val challenges = okay.acme.Acme.Challenges.Memory()
    val plan =
      for
        a <- parse(args)
        // the certificate is asked for BEFORE the server binds, so the
        // first start of an ACME site is the one that earns it
        outcome <- acmeRun(a, challenges)
        _ = outcome.foreach {
          case okay.acme.Acme.Outcome.Issued(ds, notAfter) =>
            println(s"okay-script: a certificate for ${ds.mkString(", ")} until $notAfter")
          case okay.acme.Acme.Outcome.Current(notAfter) =>
            println(s"okay-script: the certificate on disk is good until $notAfter")
        }
        ssl <- sslOf(a)
      yield (a, ssl)
    plan match
      case Left(msg) =>
        System.err.println(msg)
        System.exit(2)
      case Right((a, ssl)) =>
        // every generic codec door (sessions, Live state, persisted
        // configs, JSON bodies) answers the staged codec from here on;
        // the interpreter when the launch switch says so (staging-seam)
        if okay.staging.RuntimeStaged.install() then
          println("okay-script: staged codecs installed (okay-staging; -Dokay.staging=off keeps the interpreter)")
        else println("okay-script: staged codecs off (okay.staging=off), the interpreter serves")
        val s = site(a)
        try
          // compile the whole directory before the first visitor does
          // (docs/benchmarks.md §19: the first page of a process costs
          // ~870 ms). A broken page is NAMED here and still serves its
          // error page; the site does not refuse to start over one.
          val t0 = System.nanoTime()
          val broken = s.warm()
          val warmedMs = (System.nanoTime() - t0) / 1000000
          println(s"okay-script: compiled ${s.stats.compiles} page(s) in ${warmedMs} ms")
          broken.foreach((page, errs) => System.err.println(s"okay-script: $page does not compile: ${errs.mkString("; ")}"))
          val serving = for
            server <- s.serveWith(a.port, ssl, a.ops)
            // with no proxy in front, the plaintext port's only job is
            // to send a browser to the https one (script-https-default)
            _ <- a.httpPort.filter(_ => ssl.isDefined)
              // the challenge comes FIRST: a CA fetches it over plain
              // http and follows no redirect, so a site that redirects
              // everything can never be renewed
              .map(p => okay.jetty.Jetty.serve(p)(
                challenges.routes orElse redirectTo(okay.jetty.Jetty.port(server)))().map(Some(_)))
              .getOrElse(pure(None))
          yield server
          Resource.run[Unit, Pure](serving.map { server =>
            println(s"okay-script: serving ${a.root.toAbsolutePath} at " +
              s"${a.scheme}://127.0.0.1:${okay.jetty.Jetty.port(server)}/" +
              a.data.map(d => s" (data in $d)").getOrElse("") +
              (if a.ops then " (+ /healthz /stats /metrics)" else ""))
            a.httpPort.filter(_ => ssl.isDefined)
              .foreach(p => println(s"okay-script: port $p redirects to https"))
            Thread.sleep(Long.MaxValue)
          }).runWith
        catch case _: InterruptedException => ()
        finally s.close()
