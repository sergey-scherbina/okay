package okay.deploy

import okay.codec.{Json, Schema}
import okay.conf.Secret

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * A whole deployable system as ONE value (specs/deployment.md): the
 * services, and what each of them needs.
 *
 * `Deploy` (specs/deploy.md) said what a single process is; this says
 * what a system is, and it is the value every target renders from —
 * a laptop's compose file, a server's units, a cluster's chart, a
 * cloud's Terraform. A `Need` says WHAT ("this service needs a
 * Postgres"), never HOW; how is the target's business, and the
 * distance between those two is the whole reason this type exists.
 *
 * Everything here has a derived `Schema`, and that is load-bearing
 * rather than decorative: `render` writes the value out as
 * `deployment.json` and the `okay deploy` CLI reads THAT, so an
 * artifacts directory copied to a server works with no repository,
 * no sbt and no compiler. What the JSON cannot carry, the model may
 * not hold.
 */
final case class Deployment(
  name: String,
  services: Vector[Service],
  /** files a target should carry that the model cannot express --
   * the escape hatch, per target, deliberately awkward: an
   * application reaching for it often is telling us the model is
   * wrong, and that signal is worth keeping visible */
  extra: Vector[Extra] = Vector.empty,
):
  def service(n: String): Option[Service] = services.find(_.name == n)

  /** the services in the order a target must bring them up: what is
   * needed before what needs it. A cycle is a refusal naming it --
   * two services that each wait for the other is a deployment that
   * can never start, and better said here than at 3am. */
  def ordered: Either[String, Vector[Service]] =
    def go(done: Vector[Service], left: Vector[Service], stuck: Int): Either[String, Vector[Service]] =
      if left.isEmpty then Right(done)
      else if stuck > left.length then
        Left(s"these services wait on each other and none can start first: ${left.map(_.name).mkString(", ")}")
      else
        val ready = left.filter(s => s.neighbours.forall(n => done.exists(_.name == n) || !services.exists(_.name == n)))
        if ready.isEmpty then go(done, left, stuck + 1)
        else go(done ++ ready, left.filterNot(r => ready.exists(_.name == r.name)), 0)
    go(Vector.empty, services, 0)

final case class Extra(target: String, path: String, content: String)

final case class Service(
  name: String,
  run: Run,
  settings: Settings = Settings.empty,
  /** secret REFERENCES this service reads; no target ever renders a
   * value, only the plumbing that lets the process resolve one */
  secrets: Vector[Secret] = Vector.empty,
  needs: Vector[Need] = Vector.empty,
  health: Health = Health(),
  scale: Scale = Scale(),
  resources: Option[Resources] = None,
):
  def ports: Vector[Need.Port] = needs.collect { case p: Need.Port => p }
  def volumes: Vector[Need.Volume] = needs.collect { case v: Need.Volume => v }
  def neighbours: Vector[String] = needs.collect { case Need.Neighbour(s) => s }
  def databases: Vector[Need.Database] = needs.collect { case d: Need.Database => d }
  def caches: Vector[Need.Cache] = needs.collect { case c: Need.Cache => c }
  def dns: Vector[String] = needs.collect { case Need.Dns(h) => h }
  def region: Option[String] = needs.collectFirst { case Need.Region(r) => r }
  def tls: Option[TlsMode] = needs.collectFirst { case Need.Tls(m) => m }
  /** the one port a target exposes when it must pick one */
  def mainPort: Option[Int] = ports.find(_.public).map(_.number).orElse(ports.headOption.map(_.number))

/** what a service IS, in the two forms every target understands */
enum Run:
  /** built from this repository: an sbt module and its fat jar */
  case Module(module: String, moduleDir: String, mainClass: String, javaOpts: String = "")
  /** already an image somewhere */
  case Image(repository: String, tag: String = "latest")

enum Need:
  case Volume(path: String, size: String = "1Gi", name: String = "data")
  case Database(engine: Engine, version: String, database: String, as: String = "db")
  case Cache(engine: Engine, version: String, as: String = "cache")
  /** the name this service answers on */
  case Dns(host: String)
  case Tls(mode: TlsMode)
  /** another service in this same Deployment */
  case Neighbour(service: String)
  /** where to run: every PaaS and every cloud asks, and no target can
   * invent an answer (specs/deployment.md, stage 2) */
  case Region(name: String)
  case Port(number: Int, public: Boolean = true)

enum Engine:
  case Postgres, Redis, Mongo, Kafka

  /** the image a container target runs it as -- the ONE place a
   * default version lives, so a target does not invent one */
  def image(version: String): String = this match
    case Postgres => s"postgres:$version"
    case Redis => s"redis:$version"
    case Mongo => s"mongo:$version"
    case Kafka => s"bitnami/kafka:$version"

  def port: Int = this match
    case Postgres => 5432
    case Redis => 6379
    case Mongo => 27017
    case Kafka => 9092

enum TlsMode:
  case None, SelfSigned, Files, Acme, Proxy

/** `Health` and `Resources` are the ones specs/deploy.md already
 * defined and every existing deployment already uses -- the new model
 * REUSES them rather than minting a second pair with the same
 * meaning, which is the two-names-for-one-thing drift this repository
 * has a rule against. */
final case class Scale(replicas: Int = 1)

/**
 * The application's configuration, ONCE.
 *
 * A setting declared here becomes the process's environment, the
 * compose file's `environment`, the unit's `EnvironmentFile` and (at
 * stage 1) the ConfigMap — from one list, so the port an application
 * listens on is written in one place rather than five. `of[A]`
 * derives the names from a `Schema`, which is the difference between
 * this and the second list of environment names that okay-script's
 * fourteen `OKAY_*` variables already are.
 */
final case class Settings(all: Vector[Setting] = Vector.empty):
  def env: Vector[(String, String)] = all.map(s => s.name -> s.value)
  def ++(more: Settings): Settings = Settings(all ++ more.all)

  /** the ones a deployment actually decides, out of everything the
   * program's config could hold. `Settings.of[A]` renders the whole
   * value, defaults included, and a unit file restating a program's
   * default is a lie waiting for that default to change — so a
   * deployment names what it overrides, and a name that is not in
   * the schema simply is not there to name. */
  def only(names: String*): Settings = Settings(all.filter(s => names.contains(s.name)))
  def withValue(name: String, value: String): Settings =
    Settings(all.map(s => if s.name == name then s.copy(value = value) else s))

final case class Setting(name: String, value: String, doc: String = "")

object Settings:
  val empty: Settings = Settings()

  def of(prefix: String)(pairs: (String, String)*): Settings =
    Settings(pairs.toVector.map((k, v) => Setting(envName(prefix, k), v)))

  /**
   * The settings a config VALUE describes: its schema gives the
   * names, the value itself gives the defaults.
   *
   * Only the flat, primitive fields are settings — an environment
   * variable is a string, and a nested product would need a naming
   * convention this deliberately does not invent. A config with
   * nested shape belongs in the file layer (`Conf.load`), which
   * reads it whole.
   */
  def of[A](a: A, prefix: String)(using s: Schema[A]): Settings =
    Json.parse(Json.write(a)) match
      case Json.JObj(fields) => Settings(fields.collect {
        case (k, Json.JStr(v)) => Setting(envName(prefix, k), v)
        case (k, Json.JNum(n)) => Setting(envName(prefix, k), if n == n.toLong.toDouble then n.toLong.toString else n.toString)
        case (k, Json.JBool(b)) => Setting(envName(prefix, k), if b then "1" else "0")
      })
      case _ => Settings.empty

  /** `pages` under prefix `OKAY` is `OKAY_PAGES`; `tlsReload` is
   * `OKAY_TLS_RELOAD` -- camelCase becomes SNAKE_CASE, which is the
   * convention every environment already uses */
  // ONE derivation, in okay-conf: this renders the names and
  // Conf.layered reads them, and two lists that agree only because a
  // person keeps them agreeing is the drift being deleted here
  export okay.conf.Conf.envName

object Deployment:
  // Health, Resources and their Schemas come from specs/deploy.md's
  // own value -- imported, not redefined
  import Deploy.given

  given Schema[Secret] = Secret.given_Schema_Secret
  given Schema[Run] = Schema.derived
  given Schema[Engine] = Schema.derived
  given Schema[TlsMode] = Schema.derived
  given Schema[Need] = Schema.derived
  given Schema[Setting] = Schema.derived
  given Schema[Settings] = Schema.derived
  given Schema[Scale] = Schema.derived
  given Schema[Service] = Schema.derived
  given Schema[Extra] = Schema.derived
  given Schema[Deployment] = Schema.derived

  /** the value as the CLI reads it back */
  def json(d: Deployment): String = Json.write(d)

  def read(text: String): Either[String, Deployment] = Json.read[Deployment](text)

  /** where a target's files live: `<moduleDir>/deploy/<target>/` for
   * the first service built from this repository, or `deploy/<target>`
   * when nothing here is */
  def dir(d: Deployment, target: String): String =
    d.services.collectFirst { case Service(_, Run.Module(_, moduleDir, _, _), _, _, _, _, _, _) => moduleDir }
      .map(m => s"$m/deploy/$target").getOrElse(s"deploy/$target")

  /** every file this target would write, the value's own JSON among
   * them -- the whole deployment as data, inspectable before a byte
   * is written */
  def files(d: Deployment, target: Target): Either[String, Vector[(String, String)]] =
    target.render(d).map(_ :+ ("deployment.json" -> json(d)))

  def write(d: Deployment, target: Target, root: Path): Either[String, Vector[Path]] =
    files(d, target).map(_.map { (rel, content) =>
      val p = root.resolve(dir(d, target.name)).resolve(rel)
      Files.createDirectories(p.getParent)
      Files.writeString(p, content, UTF_8)
      p
    })

  /** the files under `root/<dir>` that differ from this rendering --
   * missing counts as differing. Empty means the committed
   * deployment IS the value. */
  def drift(d: Deployment, target: Target, root: Path): Either[String, Vector[String]] =
    files(d, target).map(_.collect { case (rel, content)
      if !Files.exists(root.resolve(dir(d, target.name)).resolve(rel)) ||
         Files.readString(root.resolve(dir(d, target.name)).resolve(rel), UTF_8) != content => rel })

/**
 * A target renders, and names the tools that apply what it rendered.
 *
 * Pure by construction: `render` is a function of the value alone, so
 * every target is testable on a machine that has none of its tools —
 * which is what makes "every place" affordable. Applying is the
 * target's own program (`docker compose`, `systemctl`), reached
 * through `up`/`down`, never reimplemented here.
 */
trait Target:
  def name: String
  def render(d: Deployment): Either[String, Vector[(String, String)]]
  /** what must exist on the machine before `up` can work */
  def requires(d: Deployment): Vector[String]
  /** the command that applies what was rendered, in that directory */
  def up(dir: Path): Vector[String]
  def down(dir: Path): Vector[String]
