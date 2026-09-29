import sbt._
import Keys._

/**
 * WHAT IS PUBLISHED, and the check that it can be (release-first-wave,
 * 2026-09-29).
 *
 * The build has well over a hundred modules, and releasing them all at
 * once would promise a compatibility nobody has looked at. The operator's
 * call: the core and the main modules go first. `names` is that wave, by
 * artifact name (`name`, before any cross suffix). Every other module
 * keeps `publishLocal` -- the guides build apps against the whole family
 * -- and is skipped by `publish`, `publishSigned` and the Central upload.
 *
 * A wave is only publishable if it is CLOSED: a wave module whose POM
 * names a compile-scope module of ours that is not in the wave cannot be
 * resolved by anybody. `releaseWaveCheck` fails on exactly that, run over
 * the whole aggregate. It also lists each wave module's third-party
 * compile dependencies, because "every dependency behind an abstraction,
 * optional" (AGENTS.md) is what a published POM finally makes visible.
 * Test-scoped dependencies are not checked: a consumer never resolves
 * them.
 *
 * As an AutoPlugin triggered for every project, a module added later is
 * outside the wave until it is named here, with nothing to remember at
 * its definition.
 */
object ReleaseWave extends AutoPlugin {
  override def trigger = allRequirements

  /** the first wave: the core, what runs it, its streams and syntax, and
   * the codec/http pair with the parser pair okay-codec needs */
  val names: Set[String] = Set(
    "okay", "okay-async", "okay-platform", "okay-stream", "okay-direct",
    "okay-optics", "okay-diagnose", "okay-test",
    "okay-lex", "okay-parse", "okay-codec", "okay-http")

  object autoImport {
    val releaseWaveCheck = taskKey[Unit]("fail when a published module depends, at compile scope, on one of ours that is not published")
  }
  import autoImport._

  private val platformOrgs = Set("org.scala-lang", "org.scala-js", "org.scala-native")

  private def compileScoped(m: ModuleID): Boolean =
    m.configurations.forall(c => c.split(';').exists(part => part == "compile" || part.startsWith("compile->") || part == "runtime" || part.startsWith("runtime->")))

  override def projectSettings: Seq[Setting[_]] = Seq(
    publish / skip := (publish / skip).value || !names(name.value),
    releaseWaveCheck := {
      val log = streams.value.log
      val me = name.value
      if ((publish / skip).value) ()
      else {
        val org = organization.value
        val (ours, theirs) = (projectDependencies.value ++ libraryDependencies.value)
          .filter(compileScoped)
          .partition(_.organization == org)
        val outside = ours.map(_.name).filterNot(names).distinct
        // the platform's own runtime is every Scala module's, and says nothing
        val foreign = theirs.filterNot(m => platformOrgs(m.organization))
          .map(m => s"${m.organization}:${m.name}").distinct.sorted
        if (foreign.nonEmpty) log.info(s"release wave: $me (${thisProject.value.id}) depends on ${foreign.mkString(", ")}")
        if (outside.nonEmpty)
          sys.error(s"release wave: $me is published but depends on ${outside.mkString(", ")}, which is not -- add it to ReleaseWave.names or drop the dependency")
      }
    }
  )
}
