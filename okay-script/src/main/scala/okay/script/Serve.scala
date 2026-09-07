package okay.script

import okay.*
import okay.given
import okay.persist.FileStore

import java.nio.file.{Files, Path, Paths}

/** The stock entry point: a directory of pages, served -- no code of
 * the caller's own. See specs/okay-script.md "Serving".
 *
 *   sbt "okayScript/runMain okay.script.Serve pages 8080"
 *   OKAY_DATA=./data sbt "okayScript/runMain okay.script.Serve pages"
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
                        httpPort: Option[Int] = None):
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
                env("OKAY_HTTP_PORT").flatMap(_.toIntOption)))
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
      case Some((cert, key)) => okay.tls.Tls.serverContext(cert, key, secrets).map(Some(_))
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
    val plan =
      for
        a <- parse(args)
        ssl <- sslOf(a)
      yield (a, ssl)
    plan match
      case Left(msg) =>
        System.err.println(msg)
        System.exit(2)
      case Right((a, ssl)) =>
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
              .map(p => okay.jetty.Jetty.serve(p)(redirectTo(okay.jetty.Jetty.port(server)))().map(Some(_)))
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
