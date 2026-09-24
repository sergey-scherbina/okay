import sbt._
import Keys._
import scala.sys.process._

/**
 * Two commands the gate runs with, so a push costs what it changed
 * and not the family (ci-affected, 2026-09-16).
 *
 * The numbers that asked for this: every Actions run in the visible
 * history was cancelled by the next push, two at the six-hour limit,
 * while the same `sbt test` takes two minutes warm on the box. A
 * docs-only lane paid for 4424 tests; a one-module lane paid for
 * ninety modules it could not have broken.
 *
 *   affected <git-ref> [task]   the projects a diff since <git-ref>
 *                               touches, closed over DEPENDENTS, and
 *                               <task> (default test) on exactly those
 *   family <jvm|js|native|all>  one platform of the whole family — the
 *                               nightly, split three ways
 *
 * A file belongs to a project when it lies under one of that
 * project's source or resource directories, NOT under its base
 * directory: the core is a `CrossType.Pure` cross project whose
 * sources live in `src/` at the root while `okayJVM`'s base is
 * `.jvm/`, and only the directories say that `src/main/scala-jvm`
 * is the JVM's and `src/main/scala` is everyone's. A change to
 * `build.sbt` or anything under `project/` is a change to every
 * project, because it is.
 *
 * Dependents are closed over the build's classpath dependencies: a
 * change in the core is a change in every module, a change in
 * okay-blob is a change in okay-blob and whatever depends on it. The
 * result is intersected with the root aggregate, which is what `sbt
 * test` at the root runs — a project the family deliberately keeps
 * out of the gate stays out.
 *
 * The closure is seeded by MAIN changes only (ci-affected-tests-only,
 * 2026-09-19): a project's own `Test` sources cannot break a
 * dependent — a dependent only ever sees what `Compile` built — so a
 * lane that edited nothing but a test still runs THAT project's own
 * tests (it stays in `direct`) but does not sweep in everything
 * downstream of it. Measured before the fix: one test-only line in
 * okay-lex (one dependent, okay-parse) pulled in 52 projects.
 *
 * Both commands run their projects through sbt's own `all`, so the
 * tasks run in parallel exactly as they do under `sbt test`.
 */
object Affected extends AutoPlugin {
  override def trigger = allRequirements
  override def globalSettings = Seq(commands ++= Seq(affected, family))

  private def under(f: File, d: File): Boolean =
    f.getAbsoluteFile.toPath.normalize.startsWith(d.getAbsoluteFile.toPath.normalize)

  /** the directories the META-BUILD compiles from source: the .sbt files in project/ name them by
   * `RootProject(file("..."))` (okay-deploy's and okay-frege's sbt plugins) */
  private def metaBuildSources(root: File): Seq[File] = {
    val pd = root / "project"
    val sbts = Option(pd.listFiles).toSeq.flatten.filter(_.getName.endsWith(".sbt"))
    sbts.flatMap { f =>
      """RootProject\(file\("([^"]+)"\)\)""".r.findAllMatchIn(IO.read(f)).map(m => (pd / m.group(1)).getCanonicalFile)
    }
  }

  /**
   * DID THIS BUILD CHANGE (affected-separate-builds). Its build is the root
   * `*.sbt`, the root `project/`, and what the meta-build compiles from
   * source. An `.sbt` anywhere else is ANOTHER build's: `okay2/` and
   * `okay-ts-browser/` are separate builds with their own `project/`, and
   * reading their `build.sbt` as this one's sent every lane that touched
   * them through the whole family (7108 tests for a docs-and-example lane,
   * measured 2026-09-24). And a plugin's `.scala` IS a change to this build,
   * which the name test missed the other way.
   */
  private def buildChanged(changed: Seq[File], root: File): Boolean = {
    val meta = metaBuildSources(root)
    val rootDir = root.getCanonicalFile
    changed.exists { f =>
      (f.getName.endsWith(".sbt") && f.getAbsoluteFile.getParentFile.getCanonicalFile == rootDir) ||
        under(f, root / "project") || meta.exists(d => under(f, d))
    }
  }

  /**
   * `<ref>`: committed AND uncommitted changes since the merge base
   * with <ref>, plus untracked files — the gate runs on the working
   * tree. `<a>..<b>`: exactly the commits between two refs, which is
   * what CI asks (`before..sha`) and what "what did that lane touch"
   * asks after the fact.
   */
  private def changedSince(base: String, root: File): Either[String, Seq[File]] =
    try {
      val lines = base.split("\\.\\.", 2) match {
        case Array(a, b) if b.nonEmpty =>
          Process(Seq("git", "diff", "--name-only", a, b), root).!!
        case _ =>
          val mergeBase = Process(Seq("git", "merge-base", base, "HEAD"), root).!!.trim
          Process(Seq("git", "diff", "--name-only", mergeBase), root).!! + "\n" +
            Process(Seq("git", "ls-files", "--others", "--exclude-standard"), root).!!
      }
      Right(lines.linesIterator.map(_.trim).filter(_.nonEmpty).map(root / _).toVector)
    } catch {
      case e: Exception => Left(s"affected: git could not resolve '$base': ${e.getMessage}")
    }

  private final class Graph(state: State) {
    val ex = Project.extract(state)
    val root: File = ex.get(ThisBuild / baseDirectory)
    val refs: Seq[ProjectRef] = ex.structure.allProjectRefs

    def resolved(r: ProjectRef): Option[ResolvedProject] =
      ex.structure.units.get(r.build).flatMap(_.defined.get(r.project))

    def dirs(r: ProjectRef): Seq[File] = mainDirs(r) ++ testDirs(r)

    /** the half of `dirs` that can break a DEPENDENT — a project's own
     * tests cannot (ci-affected-tests-only): its compiled artifact,
     * which is all a dependent ever sees, is built from `Compile`
     * alone */
    def mainDirs(r: ProjectRef): Seq[File] =
      ex.getOpt(r / Compile / unmanagedSourceDirectories).getOrElse(Nil) ++
        ex.getOpt(r / Compile / unmanagedResourceDirectories).getOrElse(Nil)

    def testDirs(r: ProjectRef): Seq[File] =
      ex.getOpt(r / Test / unmanagedSourceDirectories).getOrElse(Nil) ++
        ex.getOpt(r / Test / unmanagedResourceDirectories).getOrElse(Nil)

    /** who depends on whom, reversed: the projects a change reaches */
    val dependents: Map[ProjectRef, Set[ProjectRef]] =
      refs.flatMap(p => resolved(p).toSeq.flatMap(_.dependencies.map(d => d.project -> p)))
        .groupBy(_._1).map { case (d, ps) => d -> ps.map(_._2).toSet }

    def closeOverDependents(s: Set[ProjectRef]): Set[ProjectRef] = {
      val next = s ++ s.flatMap(p => dependents.getOrElse(p, Set.empty[ProjectRef]))
      if (next == s) s else closeOverDependents(next)
    }

    /** what `sbt test` at the root runs: the aggregate, transitively */
    val gate: Set[ProjectRef] = {
      val rootRef = ProjectRef(ex.structure.root, ex.structure.rootProject(ex.structure.root))
      def go(s: Set[ProjectRef]): Set[ProjectRef] = {
        val next = s ++ s.flatMap(p => resolved(p).toSeq.flatMap(_.aggregate))
        if (next == s) s else go(next)
      }
      go(Set(rootRef)) - rootRef
    }

    def run(state: State, projects: Set[ProjectRef], task: String): State = {
      val ids = projects.map(_.project).toSeq.sorted
      if (ids.isEmpty) { state.log.info(s"affected: nothing to run"); state }
      else {
        state.log.info(s"affected: running $task on ${ids.size} project(s): ${ids.mkString(" ")}")
        s"all ${ids.map(id => s"$id/$task").mkString(" ")}" :: state
      }
    }
  }

  /**
   * The platform filter, shared by `affected` and `family` — one
   * spelling of "what counts as JVM", because two would drift and
   * the JVM arm is the one a two-phase gate leans on.
   *
   * A project with no platform suffix (okay-script, okay-demo, the
   * jdbc bridges) counts as JVM: it is JVM-only by construction, and
   * leaving it out of the first phase would mean a lane that touches
   * it learns nothing until the expensive phase runs.
   */
  private def onPlatform(id: String, platform: String): Boolean = platform match {
    case "all" => true
    case "jvm" => id.endsWith("JVM") || !(id.endsWith("JS") || id.endsWith("Native"))
    case "js" => id.endsWith("JS")
    case "native" => id.endsWith("Native")
    case "rest" => id.endsWith("JS") || id.endsWith("Native")
    case _ => true
  }

  lazy val affected: Command = Command.args("affected", "<git-ref> [task] [jvm|js|native|rest|all]") { (state, args) =>
    val base = args.headOption.getOrElse("origin/master")
    val task = args.drop(1).headOption.getOrElse("test")
    val platform = args.drop(2).headOption.getOrElse("all").toLowerCase
    val g = new Graph(state)
    changedSince(base, g.root) match {
      case Left(why) =>
        state.log.error(why); state.fail
      case Right(changed) =>
        val buildChanged = Affected.buildChanged(changed, g.root)
        val direct: Set[ProjectRef] =
          if (buildChanged) g.gate
          else g.refs.filter { r => val ds = g.dirs(r); changed.exists(f => ds.exists(d => under(f, d))) }.toSet
        // a project whose diff touched only its TESTS cannot have
        // broken a dependent (ci-affected-tests-only): nothing under
        // Compile moved for it, so the dependent closure is seeded by
        // MAIN changes only. `direct` still runs its own tests either
        // way — this only stops a leaf's test-only edit from paying
        // for everything downstream of it.
        val mainChanged: Set[ProjectRef] =
          if (buildChanged) g.gate
          else g.refs.filter { r => val ds = g.mainDirs(r); changed.exists(f => ds.exists(d => under(f, d))) }.toSet
        val all = ((direct ++ g.closeOverDependents(mainChanged)) intersect g.gate)
          .filter(r => onPlatform(r.project, platform))
        val outside = changed.filterNot(f => g.refs.exists(r => g.dirs(r).exists(d => under(f, d))))
        state.log.info(s"affected: ${changed.size} file(s) changed since $base" +
          (if (buildChanged) " — the BUILD changed, so every project is" else
            s": ${direct.size} project(s) directly, ${all.size} with dependents") +
          (if (outside.nonEmpty && !buildChanged) s"; ${outside.size} file(s) belong to no project" else ""))
        g.run(state, all, task)
    }
  }

  lazy val family: Command = Command.args("family", "<jvm|js|native|all> [task]") { (state, args) =>
    val platform = args.headOption.getOrElse("all").toLowerCase
    val task = args.drop(1).headOption.getOrElse("test")
    val g = new Graph(state)
    if (!Set("all", "jvm", "js", "native", "rest")(platform))
      state.log.error(s"family: '$platform' is not a platform (jvm, js, native, rest, all)")
    val chosen = g.gate.filter(r => onPlatform(r.project, platform))
    g.run(state, chosen, task)
  }
}
