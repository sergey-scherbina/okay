package okay.deploy

import okay.codec.Json

/**
 * The clean machine (specs/deployment.md): what is missing, why the
 * deployment needs it, and the one command that installs it HERE.
 *
 * The whole point is the failure this replaces — `docker: command not
 * found` deep inside a compose call, or exit 127 with an empty
 * stderr. A target already declares what it `requires`, so the answer
 * costs almost nothing; what costs something is saying it well, which
 * is what `Report.table` is for.
 *
 * Probing is injected (`check(..., probe)`), so every row of the
 * report — including the ones a real machine would never produce, a
 * too-old docker, a kubectl with no context — is testable on a
 * machine that has none of these tools.
 */
object Doctor:

  /** one tool the deployment needs, and every reason it needs it */
  final case class Want(name: String, why: Vector[String])

  final case class Row(name: String, why: Vector[String], tool: Option[Tool], presence: Presence)

  final case class Report(
    target: String,
    os: String,
    arch: String,
    manager: Manager,
    rows: Vector[Row],
  ):
    def ready: Boolean = rows.forall(_.presence.ok)
    def problems: Vector[Row] = rows.filterNot(_.presence.ok)

    /** the table, and it is the whole report. Three properties are
     * requirements rather than taste: every missing tool says WHY,
     * every install line is the command for THIS machine, and the
     * last lines say what happened and what to do next. */
    def table: String =
      val sb = new StringBuilder
      sb ++= s"okay-deploy: target `$target` on $os ($arch), package manager: ${manager.label}\n\n"
      val w = math.max(16, rows.map(_.name.length).maxOption.getOrElse(0) + 2)
      sb ++= "  " + "tool".padTo(w, ' ') + "state".padTo(13, ' ') + "note\n"
      for r <- rows do
        val note = r.why.headOption.getOrElse(r.tool.map(_.why).getOrElse(""))
        sb ++= "  " + r.name.padTo(w, ' ') + r.presence.word.padTo(13, ' ') + note + "\n"
        val indent = "  " + " " * (w + 13)
        for extra <- r.why.drop(1) do sb ++= indent + extra + "\n"
        r.presence match
          case Presence.NotReady(why, fix) =>
            sb ++= indent + why + "\n" + indent + "fix:      " + fix + "\n"
          case Presence.TooOld(found, needed) =>
            sb ++= indent + s"$found is installed and this deployment needs $needed or newer\n"
          case Presence.Unknown =>
            sb ++= indent + "no tool by that name is in okay-deploy's catalogue — " +
              "the target asked for something the doctor cannot check\n"
          case _ => ()
        if !r.presence.ok then
          r.tool.foreach { t =>
            t.installWith(manager).foreach(c => sb ++= indent + "install:  " + c + "\n")
            if t.installWith(manager).isEmpty && t.note.nonEmpty then sb ++= indent + t.note + "\n"
            if t.installWith(manager).isEmpty && t.note.isEmpty then
              sb ++= indent + (
                if manager == Manager.Manual then "no package manager was detected here — install it by hand\n"
                else s"${manager.label} has no command for this one — install it by hand\n")
            if t.site.nonEmpty then sb ++= indent + "docs:     " + t.site + "\n"
          }
      sb ++= "\n"
      if ready then sb ++= s"all ${rows.length} tools are ready.\n"
      else
        sb ++= s"${problems.length} of ${rows.length} tools are not ready. Nothing has been applied.\n"
        val installable = problems.count(r => r.tool.exists(_.installWith(manager).isDefined))
        if installable > 0 then
          sb ++= s"Run with --install to install what ${manager.label} can, or install by hand and run again.\n"
        else sb ++= "Install them by hand and run again.\n"
      sb.result()

    def json: String =
      Json.print(Json.JObj(Vector(
        "target" -> Json.JStr(target),
        "os" -> Json.JStr(os),
        "arch" -> Json.JStr(arch),
        "manager" -> Json.JStr(manager.label),
        "ready" -> Json.JBool(ready),
        "tools" -> Json.JArr(rows.map { r =>
          val state = r.presence match
            case Presence.Ok(_) => "ok"
            case Presence.Missing => "missing"
            case Presence.TooOld(_, _) => "too-old"
            case Presence.NotReady(_, _) => "not-ready"
            case Presence.Unknown => "unknown"
          val extra = r.presence match
            case Presence.Ok(v) => Vector("version" -> Json.JStr(v))
            case Presence.TooOld(f, n) => Vector("version" -> Json.JStr(f), "needed" -> Json.JStr(n))
            case Presence.NotReady(w, f) => Vector("problem" -> Json.JStr(w), "fix" -> Json.JStr(f))
            case _ => Vector.empty
          Json.JObj(Vector(
            "tool" -> Json.JStr(r.name),
            "state" -> Json.JStr(state),
            "why" -> Json.JArr(r.why.map(Json.JStr(_))),
            "install" -> (r.tool.flatMap(_.installWith(manager)) match
              case Some(c) => Json.JStr(c)
              case None => Json.JNull),
            "docs" -> Json.JStr(r.tool.map(_.site).getOrElse("")),
          ) ++ extra)
        }))))

  /** every tool this deployment on this target needs, and why —
   * three sources, each carrying its own reason by construction */
  def wanted(d: Deployment, target: Target): Vector[Want] =
    val fromTarget = target.requires(d).map(n => n -> s"the `${target.name}` target needs it to apply what it renders")
    val fromSecrets = d.services.flatMap { s =>
      s.secrets.flatMap { sec =>
        val scheme = sec.ref.takeWhile(_ != ':')
        Tools.forScheme(scheme).map(t => t.name -> s"one secret is a `${sec.ref}` reference (${s.name})")
      }
    }
    val fromSops =
      if fromSecrets.exists(_._1 == Tools.sops.name) then Vector(Tools.age.name -> "sops decrypts with an age key")
      else Vector.empty
    val fromTls = d.services.flatMap { s =>
      s.tls match
        case Some(TlsMode.SelfSigned) | Some(TlsMode.Acme) =>
          Vector(Tools.openssl.name -> s"${s.name} builds a certificate (${s.tls.get})")
        case _ => Vector.empty
    }
    (fromTarget ++ fromSecrets ++ fromSops ++ fromTls)
      .groupBy(_._1).toVector
      .map((n, ps) => Want(n, ps.map(_._2).distinct))
      .sortBy(w => Tools.all.indexWhere(_.name == w.name) match { case -1 => Int.MaxValue; case i => i })

  /** what the machine says about one tool: `Missing` when the binary
   * is not there, `TooOld` when it is behind, `NotReady` when it is
   * installed and still cannot be used — the state that happens most */
  def probe(t: Tool): Presence =
    val out = Shell.run(t.probe)
    if out.code == 127 then Presence.Missing
    else
      t.version(out.text) match
        case Some(v) if t.atLeast.exists(n => !Tool.atLeast(v, n)) =>
          Presence.TooOld(v, t.atLeast.get)
        case found =>
          if !out.ok && found.isEmpty then
            Presence.NotReady(
              s"`${Shell.line(t.probe)}` exited ${out.code}",
              if out.tail(2).isEmpty then "run it by hand — it printed nothing" else out.tail(2))
          else
            t.ready match
              case Some(r) =>
                val check = Shell.run(r.probe)
                if check.ok then Presence.Ok(found.getOrElse("present"))
                else Presence.NotReady(r.why, r.fix)
              case None => Presence.Ok(found.getOrElse("present"))

  def check(d: Deployment, target: Target, probing: Tool => Presence = probe): Report =
    Report(
      target = target.name,
      os = System.getProperty("os.name", "?") + " " + System.getProperty("os.version", ""),
      arch = System.getProperty("os.arch", "?"),
      manager = Tools.manager(),
      rows = wanted(d, target).map { w =>
        Tools.byName(w.name) match
          case Some(t) => Row(w.name, w.why, Some(t), probing(t))
          case None => Row(w.name, w.why, None, Presence.Unknown)
      })

  /**
   * Installing, and the line under it: opt-in, one flag, and never
   * silent. Each command is PRINTED before it runs, we use the
   * platform's own package manager, and the four refusals from the
   * spec hold — no `curl | sh`, no silent sudo, no version pin of
   * ours, never during an apply.
   */
  def install(r: Report, echo: String => Unit, run: Vector[String] => Shell.Out = Shell.run(_)): Vector[String] =
    r.problems.flatMap { row =>
      row.tool.flatMap(t => t.installWith(r.manager).map(t -> _)) match
        case None =>
          echo(s"${row.name}: ${r.manager.label} has no command for this one — install it by hand" +
            row.tool.filter(_.site.nonEmpty).map(t => s" (${t.site})").getOrElse(""))
          None
        case Some((_, cmd)) if piped(cmd) =>
          echo(s"${row.name}: refusing to run `$cmd` — okay-deploy never pipes a downloaded script into a shell")
          None
        case Some((_, cmd)) =>
          echo(s"$$ $cmd")
          val out = run(Vector("sh", "-c", cmd))
          if out.ok then
            echo(s"${row.name}: installed")
            Some(row.name)
          else
            echo(Shell.failure(Vector("sh", "-c", cmd), out, s"installing ${row.name}"))
            None
    }

  /** the first refusal, as a predicate: a command that pipes the
   * network into a shell is never run, whatever it is for */
  def piped(cmd: String): Boolean =
    val c = cmd.toLowerCase
    (c.contains("curl") || c.contains("wget")) && c.contains("|")
