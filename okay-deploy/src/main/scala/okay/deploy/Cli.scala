package okay.deploy

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * `okay deploy` (specs/deployment.md, "The CLI, and the deployment as
 * data").
 *
 * The decision that shapes everything here: the CLI reads the
 * deployment as DATA. `render` wrote `deployment.json` — the
 * `Deployment` value through its own `Schema` — beside the target's
 * files, and every verb reads THAT rather than evaluating Scala. So
 * an artifacts directory copied to a server works with no repository,
 * no sbt and no compiler; one binary serves every deployment; and
 * what the CLI can do is exactly what the spec says a deployment is.
 *
 * `run` is the whole program and returns the exit code, so the tests
 * drive it as a function — the subprocess runner and the tool prober
 * are injected, which is what lets a machine with no docker assert on
 * what `up` would have done.
 */
object Cli:

  /** 0 applied · 1 the operation failed · 2 the arguments were wrong
   * · 3 a prerequisite is missing — the doctor's own code, so a
   * pipeline can tell "install docker" from "the deploy failed" */
  object Exit:
    val ok = 0
    val failed = 1
    val usage = 2
    val prerequisite = 3

  def main(args: Array[String]): Unit =
    val code = run(args.toVector, println, System.err.println, Path.of("").toAbsolutePath)
    if code != Exit.ok then sys.exit(code)

  val help: String =
    """okay deploy — one deployment value, applied where you need it
      |
      |  okay deploy up <target>          check, render if stale, apply
      |  okay deploy doctor [target]      what this machine is missing, and how to fix it
      |  okay deploy diff <target>        the committed files vs what the value renders
      |
      |  okay deploy render <target>      write the target's files from deployment.json
      |  okay deploy down <target>        stop what `up` started
      |  okay deploy targets              what this deployment can be applied to
      |
      |options
      |  --file <deployment.json>  the deployment to read (default: found near the working directory)
      |  --dry-run                 print the commands instead of running them
      |  --json                    machine-readable output, same fields as the table
      |  --install                 doctor only: install what the detected package manager can
      |  --yes                     accepted; this CLI never prompts, so it changes nothing
      |
      |exit codes: 0 applied · 1 failed · 2 bad arguments · 3 a prerequisite is missing
      |""".stripMargin

  def run(
    args: Vector[String],
    out: String => Unit,
    err: String => Unit,
    cwd: Path,
    exec: Vector[String] => Shell.Out = Shell.run(_),
    probing: Tool => Presence = Doctor.probe,
  ): Int =
    val flags = args.filter(_.startsWith("--"))
    // `okay deploy up laptop` and `okay-deploy up laptop` are the same
    // program: the group name is how this grows a sibling later
    // (`okay script serve`), and dropping it here costs nothing
    val words = args.filterNot(_.startsWith("--")).filterNot(w => valueOf(args, "--file").contains(w)) match
      case "deploy" +: rest => rest
      case all => all
    val unknown = flags.map(_.takeWhile(_ != '=')).filterNot(known.contains)
    if unknown.nonEmpty then
      err(s"unknown option${if unknown.length > 1 then "s" else ""}: ${unknown.mkString(", ")}")
      err(help)
      return Exit.usage

    val dryRun = flags.contains("--dry-run")
    val asJson = flags.contains("--json")
    val file = valueOf(args, "--file").map(cwd.resolve)

    words.headOption match
      case None | Some("help") | Some("-h") =>
        out(help); if words.isEmpty then Exit.usage else Exit.ok

      case Some("targets") =>
        located(file, cwd, None, err) match
          case Left(code) => code
          case Right((d, _, _)) =>
            if asJson then
              out(okay.codec.Json.print(okay.codec.Json.JArr(Targets.all.map { t =>
                okay.codec.Json.JObj(Vector(
                  "target" -> okay.codec.Json.JStr(t.name),
                  "renders" -> okay.codec.Json.JBool(t.render(d).isRight),
                  "requires" -> okay.codec.Json.JArr(t.requires(d).map(okay.codec.Json.JStr(_))),
                  "refused" -> (t.render(d) match
                    case Left(m) => okay.codec.Json.JStr(m)
                    case Right(_) => okay.codec.Json.JNull)))
              })))
            else
              out(s"${d.name}: ${d.services.length} service${if d.services.length == 1 then "" else "s"}")
              for t <- Targets.all do
                t.render(d) match
                  case Right(fs) =>
                    out(f"  ${t.name}%-10s ${fs.length} file${if fs.length == 1 then "" else "s"}, " +
                      s"needs ${t.requires(d).mkString(", ")}")
                  case Left(m) => out(f"  ${t.name}%-10s cannot: $m")
            Exit.ok

      case Some(verb) =>
        val target = words.lift(1)
        if !Vector("render", "doctor", "up", "down", "diff").contains(verb) then
          err(s"unknown command: $verb"); err(help); Exit.usage
        else
          located(file, cwd, target, err) match
            case Left(code) => code
            case Right((d, dir, chosen)) =>
              Targets.byName(chosen) match
                case None if target.isEmpty =>
                  // the directory the JSON sits in is not a target's
                  // name, so there is nothing to infer from
                  err(s"`okay deploy $verb` needs a target — this build has ${Targets.all.map(_.name).mkString(", ")}")
                  Exit.usage
                case None =>
                  err(s"no target named `$chosen` — this build has ${Targets.all.map(_.name).mkString(", ")}")
                  Exit.usage
                case Some(t) => verb match
                  case "doctor" => doctor(d, t, flags.contains("--install"), asJson, out, exec, probing)
                  case "diff" => diff(d, t, dir, asJson, out)
                  case "render" => render(d, t, dir, dryRun, out, err)
                  case "up" => up(d, t, dir, dryRun, out, err, exec, probing)
                  case "down" => apply_(t.down(dir), dryRun, out, err, exec, s"stopping ${d.name} on ${t.name}")
                  case _ => Exit.usage

  // ------------------------------------------------------------------
  // the verbs
  // ------------------------------------------------------------------

  private def doctor(
    d: Deployment, t: Target, installing: Boolean, asJson: Boolean,
    out: String => Unit, exec: Vector[String] => Shell.Out, probing: Tool => Presence,
  ): Int =
    val first = Doctor.check(d, t, probing)
    if asJson then
      out(first.json)
      if first.ready then Exit.ok else Exit.prerequisite
    else
      out(first.table)
      if first.ready || !installing then (if first.ready then Exit.ok else Exit.prerequisite)
      else
        // installation is its own step, before anything is rendered
        // or applied -- half-applying and then installing is the
        // worst of both (specs)
        out("installing what " + first.manager.label + " can. Each command is printed before it runs.\n")
        Doctor.install(first, out, exec): Unit
        out("")
        val again = Doctor.check(d, t, probing)
        out(again.table)
        if again.ready then Exit.ok else Exit.prerequisite

  private def diff(d: Deployment, t: Target, dir: Path, asJson: Boolean, out: String => Unit): Int =
    driftOf(d, t, dir) match
      case Left(m) => out(m); Exit.failed
      case Right(Vector()) =>
        if asJson then out("""{"drift":[]}""") else out(s"`${t.name}` is what the value renders — no drift.")
        Exit.ok
      case Right(files) =>
        if asJson then
          out(okay.codec.Json.print(okay.codec.Json.JObj(Vector(
            "drift" -> okay.codec.Json.JArr(files.map(okay.codec.Json.JStr(_)))))))
        else
          out(s"${files.length} file${if files.length == 1 then " under" else "s under"} $dir " +
            (if files.length == 1 then "differs" else "differ") + " from the value:")
          files.foreach(f => out(s"  $f"))
          out("run `okay deploy render " + t.name + "` to bring them back.")
        Exit.failed

  private def render(d: Deployment, t: Target, dir: Path, dryRun: Boolean, out: String => Unit, err: String => Unit): Int =
    Deployment.files(d, t) match
      case Left(m) => err(s"the `${t.name}` target refuses this deployment: $m"); Exit.failed
      case Right(files) =>
        val changed = files.filter { (rel, content) =>
          val p = dir.resolve(rel)
          !Files.exists(p) || Files.readString(p, UTF_8) != content
        }
        if dryRun then
          out(s"would write ${changed.length} of ${files.length} files under $dir:")
          changed.foreach((rel, _) => out(s"  $rel"))
        else
          for (rel, content) <- files do
            val p = dir.resolve(rel)
            Option(p.getParent).foreach(Files.createDirectories(_): Unit)
            Files.writeString(p, content, UTF_8): Unit
          if changed.isEmpty then out(s"${files.length} files under $dir were already what the value renders.")
          else
            out(s"wrote ${changed.length} of ${files.length} files under $dir:")
            changed.foreach((rel, _) => out(s"  $rel"))
        Exit.ok

  private def up(
    d: Deployment, t: Target, dir: Path, dryRun: Boolean,
    out: String => Unit, err: String => Unit, exec: Vector[String] => Shell.Out, probing: Tool => Presence,
  ): Int =
    // the check comes BEFORE anything is rendered or applied: a
    // machine without docker gets the report and nothing else
    val report = Doctor.check(d, t, probing)
    if !report.ready then
      out(report.table)
      err(s"`up` needs the tools above. Nothing has been applied.")
      Exit.prerequisite
    else
      driftOf(d, t, dir) match
        case Left(m) => err(m); Exit.failed
        case Right(files) =>
          if files.nonEmpty then
            out(s"${files.length} rendered file${if files.length == 1 then " is" else "s are"} stale; re-rendering first.")
            val code = render(d, t, dir, dryRun, out, err)
            if code != Exit.ok then return code
          apply_(t.up(dir), dryRun, out, err, exec, s"applying ${d.name} to ${t.name}")

  private def apply_(
    cmd: Vector[String], dryRun: Boolean,
    out: String => Unit, err: String => Unit, exec: Vector[String] => Shell.Out, what: String,
  ): Int =
    if dryRun then
      out(s"would run: ${Shell.line(cmd)}")
      Exit.ok
    else
      out(s"$$ ${Shell.line(cmd)}")
      val res = exec(cmd)
      if res.ok then
        if res.text.trim.nonEmpty then out(res.text.trim)
        out(s"$what: done.")
        Exit.ok
      else
        // every failure carries the wrapped command -- the rule from
        // "No silent failure, anywhere", and the CLI is where an
        // operator meets it
        err(Shell.failure(cmd, res, what))
        Exit.failed

  // ------------------------------------------------------------------
  // finding the deployment
  // ------------------------------------------------------------------

  /** the same question `Deploy.drift` answers, asked about the
   * directory the JSON was found in rather than a repository root —
   * on a server there is no root */
  private def driftOf(d: Deployment, t: Target, dir: Path): Either[String, Vector[String]] =
    Deployment.files(d, t).map(_.collect {
      case (rel, content) if !Files.exists(dir.resolve(rel)) || Files.readString(dir.resolve(rel), UTF_8) != content => rel
    })

  private val known = Vector("--file", "--dry-run", "--json", "--install", "--yes", "--help")

  private def valueOf(args: Vector[String], flag: String): Option[String] =
    args.indexOf(flag) match
      case -1 => args.find(_.startsWith(flag + "=")).map(_.drop(flag.length + 1))
      case i => args.lift(i + 1)

  /** where a deployment.json can be, in the order an operator would
   * expect: the one they named, the directory they are standing in,
   * the target's directory under it, and one level down — which is
   * what a repository checkout looks like (`<module>/deploy/<t>/`) */
  def candidates(cwd: Path, target: Option[String]): Vector[Path] =
    val here = Vector(cwd.resolve("deployment.json"))
    val under = target.toVector.map(t => cwd.resolve("deploy").resolve(t).resolve("deployment.json"))
    val anyUnder =
      if target.isDefined then Vector.empty
      else listDirs(cwd.resolve("deploy")).map(_.resolve("deployment.json"))
    val deeper =
      listDirs(cwd).flatMap { m =>
        val base = m.resolve("deploy")
        target match
          case Some(t) => Vector(base.resolve(t).resolve("deployment.json"))
          case None => listDirs(base).map(_.resolve("deployment.json"))
      }
    (here ++ under ++ anyUnder ++ deeper).distinct

  private def listDirs(p: Path): Vector[Path] =
    if !Files.isDirectory(p) then Vector.empty
    else
      val s = Files.list(p)
      try s.iterator.asScala.toVector.filter(Files.isDirectory(_)).sortBy(_.getFileName.toString)
      finally s.close()

  /** the deployment, the directory its files live in, and the target
   * name — from the JSON's own path when the operator did not name
   * one, which is how `okay deploy up` works inside `deploy/host/` */
  private def located(
    file: Option[Path], cwd: Path, target: Option[String], err: String => Unit,
  ): Either[Int, (Deployment, Path, String)] =
    val tried = file.map(Vector(_)).getOrElse(candidates(cwd, target))
    tried.find(Files.isRegularFile(_)) match
      case None =>
        err("no deployment.json found. Looked in:")
        tried.foreach(p => err(s"  $p"))
        err("Render one from the build first: sbt \"<app>/runMain <app>.Deploy\" — " +
          "or copy the deploy/ directory here and run from beside it.")
        Left(Exit.usage)
      case Some(p) =>
        Deployment.read(Files.readString(p, UTF_8)) match
          case Left(m) =>
            err(s"$p is not a deployment this build understands: $m")
            Left(Exit.failed)
          case Right(d) =>
            val dir = p.getParent
            val name = target.getOrElse(dir.getFileName.toString)
            Right((d, dir, name))
