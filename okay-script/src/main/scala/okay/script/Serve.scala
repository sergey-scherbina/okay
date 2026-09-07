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
 * `OKAY_DATA` names a directory for an okay-persist `FileStore`: with
 * it the sessions (`Sessions.persisted`) and the application scope
 * (`Application.persisted`) survive a restart; without it both are in
 * memory. Runs until interrupted (Ctrl-C, a SIGTERM): the `Resource`
 * releases and the server stops -- the lifecycle proof, applied.
 */
object Serve:

  final case class Args(root: Path, port: Int, data: Option[Path])

  /** `<dir> [port]`; port 8080 by default */
  def parse(args: Array[String], env: String => Option[String] = k => Option(System.getenv(k))): Either[String, Args] =
    args.toList match
      case dir :: rest if rest.length <= 1 =>
        val root = Paths.get(dir)
        if !Files.isDirectory(root) then Left(s"not a directory: $dir")
        else
          rest.headOption.map(_.toIntOption.toRight(s"not a port: ${rest.head}")).getOrElse(Right(8080))
            .map(port => Args(root, port, env("OKAY_DATA").map(Paths.get(_))))
      case _ => Left("usage: okay.script.Serve <pages-dir> [port]   (OKAY_DATA=<dir> for a persistent store)")

  /** the Site the arguments describe -- a caller wanting `verify`/
   * `issue` or a shared `Sessions` builds its own from here */
  def site(a: Args): Site =
    a.data match
      case None => Site(a.root)
      case Some(dir) =>
        Files.createDirectories(dir)
        val store = FileStore.open(dir)
        Site(a.root, sessions = Sessions.persisted(store), application = api.Application.persisted(store))

  def main(args: Array[String]): Unit =
    parse(args) match
      case Left(msg) =>
        System.err.println(msg)
        System.exit(2)
      case Right(a) =>
        val s = site(a)
        try
          Resource.run[Unit, Pure](s.serve(a.port).map { server =>
            println(s"okay-script: serving ${a.root.toAbsolutePath} at http://127.0.0.1:${okay.jetty.Jetty.port(server)}/" +
              a.data.map(d => s" (data in $d)").getOrElse(""))
            Thread.sleep(Long.MaxValue)
          }).runWith
        catch case _: InterruptedException => ()
        finally s.close()
