package okay.script

import okay.codec.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * `okay script` (specs/okay-script.md, "The command line").
 *
 * Serving is one thing a document that compiles is for, and not the
 * most common: this runs one, renders one, builds a whole directory
 * into a static site, checks a document's ` ```stdout ` fences the way
 * mdoc does, serves a directory, and writes a starter that already
 * works.
 *
 * It is ALSO the combined `okay` binary. okay-deploy cannot depend on
 * okay-script — the arrow points the other way — so `okay script …`
 * could never have lived in okay-deploy's jar; okay-script's own
 * assembly already carries okay-deploy, so this dispatches `deploy` to
 * `okay.deploy.Cli` and okay-deploy's smaller jar stays deploy-only
 * for an operator who wants nothing else.
 *
 * `run` returns the exit code so the tests drive it as a function.
 */
object Cli:

  /** the deployment CLI's codes, because an operator uses both */
  object Exit:
    val ok = 0
    val failed = 1
    val usage = 2

  def main(args: Array[String]): Unit =
    val code = run(args.toVector, println, System.err.println, Path.of("").toAbsolutePath)
    if code != Exit.ok then sys.exit(code)

  val help: String =
    """okay script — markdown files that are Scala source, run and rendered
      |
      |  okay script run    <file.md>            run its blocks; stdout is the output
      |  okay script render <file.md> [-o out]   prose and ${…} rendered; a file, or stdout
      |  okay script build  <dir> -o <out>       every page rendered ONCE: a static site
      |
      |  okay script check  <file.md>|<dir>      the ```stdout fences must match the real run
      |  okay script serve  <dir> [port]         compile per request and answer
      |  okay script new    <dir>                a starter that already works
      |
      |options
      |  -o <path>   where output goes (render: a file; build: a directory)
      |  --lang <a,b>  the languages a site speaks, the first the default
      |  --json      machine-readable output, same fields as the text
      |
      |exit codes: 0 fine · 1 the operation failed · 2 bad arguments
      |""".stripMargin

  def run(args: Vector[String], out: String => Unit, err: String => Unit, cwd: Path): Int =
    val words = args.filterNot(_.startsWith("-")).filterNot(w => valueOf(args, "-o").contains(w))
      .filterNot(w => valueOf(args, "--lang").contains(w))
    val asJson = args.contains("--json")
    val target = valueOf(args, "-o").map(cwd.resolve)
    val languages = valueOf(args, "--lang").map(_.split(',').toVector.map(_.trim).filter(_.nonEmpty))
      .filter(_.nonEmpty).getOrElse(Vector("en"))

    words match
      // the combined binary: okay-script's jar carries okay-deploy, so
      // one `okay` answers both groups
      case "deploy" +: _ =>
        okay.deploy.Cli.run(args, out, err, cwd)

      case Vector() | Vector("help") | Vector("-h") =>
        out(help)
        if words.isEmpty then Exit.usage else Exit.ok

      case "script" +: rest => verb(rest, out, err, cwd, target, languages, asJson)
      case all => verb(all, out, err, cwd, target, languages, asJson)

  private def verb(
    words: Vector[String], out: String => Unit, err: String => Unit, cwd: Path,
    target: Option[Path], languages: Vector[String], asJson: Boolean,
  ): Int =
    words match
      case Vector("run", file) => runOne(cwd.resolve(file), out, err)
      case Vector("render", file) => render(cwd.resolve(file), target, out, err)
      case Vector("build", dir) => build(cwd.resolve(dir), target, languages, out, err, asJson)
      case Vector("check", what) => check(cwd.resolve(what), out, err, asJson)
      case Vector("serve", rest*) if rest.length <= 2 => serve(rest.toVector)
      case Vector("new", dir) => starter(cwd.resolve(dir), out, err)
      case Vector(v, _*) if Vector("run", "render", "build", "check", "new").contains(v) =>
        err(s"`okay script $v` takes one path")
        Exit.usage
      case Vector(v, _*) =>
        err(s"unknown command: $v")
        err(help)
        Exit.usage
      case Vector() =>
        out(help); Exit.usage

  // ------------------------------------------------------------------

  private def readable(f: Path, err: String => Unit): Option[String] =
    if Files.isRegularFile(f) then Some(Files.readString(f, UTF_8))
    else
      err(s"not a file: $f")
      None

  private def runOne(f: Path, out: String => Unit, err: String => Unit): Int =
    readable(f, err) match
      case None => Exit.usage
      case Some(text) =>
        val r = ScalaScript.run(text)
        if r.stdout.nonEmpty then out(r.stdout.stripSuffix("\n"))
        if r.ok then Exit.ok
        else
          // a compile error already knows the file and the line
          // (okay-script-line-mapping), so the message is the message
          r.errors.foreach(e => err(s"${f.getFileName}: $e"))
          r.thrown.foreach(t => err(s"${f.getFileName}: threw ${t.getClass.getName}: ${t.getMessage}"))
          Exit.failed

  private def render(f: Path, target: Option[Path], out: String => Unit, err: String => Unit): Int =
    readable(f, err) match
      case None => Exit.usage
      case Some(text) =>
        val r = ScalaScript.render(text)
        if !r.ok then
          r.errors.foreach(e => err(s"${f.getFileName}: $e"))
          r.thrown.foreach(t => err(s"${f.getFileName}: threw ${t.getClass.getName}: ${t.getMessage}"))
          Exit.failed
        else
          target match
            case None => out(r.stdout.stripSuffix("\n")); Exit.ok
            case Some(p) =>
              Option(p.getParent).foreach(Files.createDirectories(_): Unit)
              Files.writeString(p, r.stdout, UTF_8): Unit
              out(s"wrote $p")
              Exit.ok

  private def build(
    dir: Path, target: Option[Path], languages: Vector[String],
    out: String => Unit, err: String => Unit, asJson: Boolean,
  ): Int =
    if !Files.isDirectory(dir) then
      err(s"not a directory: $dir")
      Exit.usage
    else target match
      case None =>
        err("`okay script build` needs somewhere to put the site: -o <dir>")
        Exit.usage
      case Some(o) =>
        val report = Build.run(dir, o, languages)
        if asJson then
          val page = (p: Build.Page) => Json.JObj(Vector(
            "page" -> Json.JStr(p.path),
            "out" -> Json.JStr(p.out),
            "lang" -> Json.JStr(p.lang)))
          val note = (pw: (String, String)) => Json.JObj(Vector(
            "page" -> Json.JStr(pw._1),
            "why" -> Json.JStr(pw._2)))
          val doc = Json.JObj(Vector(
            "ok" -> Json.JBool(report.ok),
            "written" -> Json.JArr(report.written.map(page)),
            "copied" -> Json.JArr(report.copied.map(Json.JStr(_))),
            "skipped" -> Json.JArr(report.skipped.map(note)),
            "failed" -> Json.JArr(report.failed.map(note))))
          out(Json.print(doc))
        else
          for p <- report.written do out(s"  ${p.out}")
          if report.copied.nonEmpty then out(s"  ${report.copied.length} static files copied")
          for (p, why) <- report.skipped do out(s"skipped $p — $why")
          for (p, why) <- report.failed do err(s"$p: $why")
          if report.ok then
            out(s"${report.written.length} pages into $o. Serve that directory with anything; no JVM needed.")
          else
            err(s"${report.failed.length} of ${report.failed.length + report.written.length} pages could not be built.")
        if report.ok then Exit.ok else Exit.failed

  private def check(what: Path, out: String => Unit, err: String => Unit, asJson: Boolean): Int =
    val files =
      if Files.isDirectory(what) then
        val walk = Files.walk(what)
        try walk.iterator().asScala.toVector
          .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".md")).sortBy(_.toString)
        finally walk.close()
      else if Files.isRegularFile(what) then Vector(what)
      else Vector.empty

    if files.isEmpty then
      err(s"no markdown to check at $what")
      Exit.usage
    else
      val results = files.map(f => f -> ScalaScript.check(Files.readString(f, UTF_8)))
      val bad = results.filterNot(_._2.ok)
      if asJson then
        val one = (fr: (Path, CheckResult)) => Json.JObj(Vector(
          "file" -> Json.JStr(fr._1.toString),
          "mismatches" -> Json.JArr(fr._2.mismatches.map(Json.JStr(_)))))
        val doc = Json.JObj(Vector(
          "ok" -> Json.JBool(bad.isEmpty),
          "checked" -> Json.JNum(files.length.toDouble),
          "failed" -> Json.JArr(bad.map(one))))
        out(Json.print(doc))
      else
        for (f, r) <- results do
          if r.ok then out(s"ok   ${f.getFileName}")
          else
            err(s"FAIL ${f}")
            r.mismatches.foreach(m => err(s"       $m"))
        if bad.isEmpty then out(s"${files.length} checked, all matching.")
      if bad.isEmpty then Exit.ok else Exit.failed

  /** `serve` IS `Serve`: the same main, the same environment, the
   * same ACME and TLS road — a second implementation would be a
   * second set of switches to keep in step */
  private def serve(rest: Vector[String]): Int =
    Serve.main(rest.toArray)
    Exit.ok

  private def starter(dir: Path, out: String => Unit, err: String => Unit): Int =
    if Files.exists(dir) && Files.list(dir).findAny().isPresent then
      err(s"$dir is not empty — `new` writes a starter and will not overwrite one")
      Exit.usage
    else
      Files.createDirectories(dir): Unit
      for (rel, content) <- Starter.files do
        val f = dir.resolve(rel)
        Option(f.getParent).foreach(Files.createDirectories(_): Unit)
        Files.writeString(f, content, UTF_8): Unit
        out(s"  $rel")
      out(s"\nokay script serve $dir      # and open http://localhost:8080")
      out(s"okay script build $dir -o site   # or render it once, to files")
      Exit.ok

  private def valueOf(args: Vector[String], flag: String): Option[String] =
    args.indexOf(flag) match
      case -1 => args.find(_.startsWith(flag + "=")).map(_.drop(flag.length + 1))
      case i => args.lift(i + 1)

/** What `new` writes: a site that serves AND builds with nothing to
 * edit first — which is the only test of a starter that matters. */
private object Starter:
  val files: Vector[(String, String)] = Vector(
    "index.md" ->
      """---
        |title: A page that is a program
        |---
        |<link rel="stylesheet" href="/style.css">
        |
        |<h1>${okay.script.Meta.current("title")}</h1>
        |
        |<p>This file is markdown and Scala at once: the prose is written
        |straight through, and anything in <code>${…}</code> is an
        |expression the page evaluates when it is rendered.</p>
        |
        |<p>Two plus two is ${2 + 2}, and this page speaks
        |${okay.script.api.Lang.current}.</p>
        |
        |```scala
        |val hour = java.time.LocalTime.now().getHour
        |println(s"<p>It was hour $hour when this page ran.</p>")
        |```
        |
        |<p><a href="/about">The other page</a></p>
        |""".stripMargin.replace("${…}", "$${…}"),
    "about.md" ->
      """---
        |title: About
        |---
        |<link rel="stylesheet" href="/style.css">
        |
        |<h1>${okay.script.Meta.current("title")}</h1>
        |
        |<p>A second page, so that a link between two of them is a link.</p>
        |
        |<p><a href="/">Back</a></p>
        |""".stripMargin,
    "style.css" ->
      """/* the whole stylesheet; delete it and nothing breaks */
        |body { font: 16px/1.6 system-ui, sans-serif; max-width: 42rem; margin: 3rem auto; padding: 0 1rem; }
        |h1 { font-size: 1.6rem; }
        |code { background: #f3f3f3; padding: 0 .25rem; border-radius: 3px; }
        |a { color: #06c; }
        |""".stripMargin,
  )
