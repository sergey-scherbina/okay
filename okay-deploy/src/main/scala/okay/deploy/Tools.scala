package okay.deploy

/**
 * What a check knows about one tool (specs/deployment.md, "The clean
 * machine").
 *
 * A `Tool` is a VALUE, so the whole catalogue is testable on a
 * machine that has none of these installed: probing is a function of
 * the tool and the machine, and everything else here — why it is
 * needed, what installs it, where the vendor documents it — is data
 * this module can assert on without running anything.
 */
final case class Tool(
  name: String,
  /** what says the binary is there, and prints its version */
  probe: Vector[String],
  /** read a version out of that output; the default reads the first
   * dotted number, which is what nearly every `--version` prints */
  version: String => Option[String] = Tool.firstVersion,
  /** the minimum this needs, when it has one. We only ever REPORT
   * that what is installed is too old — never pin (specs) */
  atLeast: Option[String] = None,
  /** why the deployment needs it, in the operator's terms */
  why: String,
  /** the exact command, per package manager */
  install: Map[Manager, String] = Map.empty,
  /** the probe that says it WORKS rather than merely exists — the
   * state that actually happens most */
  ready: Option[Ready] = None,
  /** a sentence for the operator when there is nothing to run: it
   * arrives with something else, or it is the machine's own init.
   * Kept apart from `install` because that map's values are COMMANDS
   * — putting English there is how a tool ends up trying to run it */
  note: String = "",
  site: String = "",
):
  def installWith(m: Manager): Option[String] = install.get(m)

/**
 * The second probe, and the sentence its failure produces.
 *
 * The spec sketched this as a bare `Option[Vector[String]]`; writing
 * it showed the probe is useless without the fix that goes with it,
 * and two parallel Options that must agree is the shape this
 * repository keeps deleting. One value carries both.
 */
final case class Ready(probe: Vector[String], why: String, fix: String)

enum Manager:
  case Brew, Apt, Dnf, Apk, Pacman, Winget, Manual

  def label: String = this match
    case Brew => "brew"
    case Apt => "apt"
    case Dnf => "dnf"
    case Apk => "apk"
    case Pacman => "pacman"
    case Winget => "winget"
    case Manual => "none detected"

enum Presence:
  case Ok(version: String)
  case Missing
  case TooOld(found: String, needed: String)
  /** installed, and still cannot be used: a stopped docker daemon, a
   * kubectl with no context, a flyctl nobody is logged into */
  case NotReady(why: String, fix: String)
  /** a target required a name the catalogue does not know — a
   * finding, never a silent pass */
  case Unknown

  def ok: Boolean = this match
    case Ok(_) => true
    case _ => false

  def word: String = this match
    case Ok(v) => s"ok $v"
    case Missing => "MISSING"
    case TooOld(f, _) => s"TOO OLD $f"
    case NotReady(_, _) => "NOT READY"
    case Unknown => "UNKNOWN"

object Tool:
  private val dotted = """(\d+\.\d+(?:\.\d+)?)""".r

  val firstVersion: String => Option[String] = out => dotted.findFirstIn(out)

  /** 1.2.10 is newer than 1.2.9 — numeric per segment, missing
   * segments are zero */
  def atLeast(found: String, needed: String): Boolean =
    def parts(s: String) = s.split('.').toVector.map(p => p.takeWhile(_.isDigit)).map(p => if p.isEmpty then 0 else p.toInt)
    val (f, n) = (parts(found), parts(needed))
    val len = math.max(f.length, n.length)
    (0 until len).view.map(i => (f.applyOrElse(i, (_: Int) => 0), n.applyOrElse(i, (_: Int) => 0)))
      .find((a, b) => a != b).forall((a, b) => a > b)

object Tools:

  val docker: Tool = Tool(
    name = "docker",
    probe = Vector("docker", "--version"),
    atLeast = Some("20.10"),
    why = "the laptop target builds and runs containers",
    install = Map(
      Manager.Brew -> "brew install --cask docker",
      Manager.Apt -> "sudo apt-get install -y docker.io",
      Manager.Dnf -> "sudo dnf install -y docker",
      Manager.Apk -> "sudo apk add docker",
      Manager.Pacman -> "sudo pacman -S --noconfirm docker",
      Manager.Winget -> "winget install Docker.DockerDesktop",
    ),
    ready = Some(Ready(
      Vector("docker", "info"),
      "the Docker daemon is not running",
      "start Docker Desktop, or `sudo systemctl start docker`")),
    site = "https://docs.docker.com/get-started/")

  val compose: Tool = Tool(
    name = "docker compose",
    probe = Vector("docker", "compose", "version"),
    why = "the laptop target applies a compose file",
    install = Map(Manager.Apt -> "sudo apt-get install -y docker-compose-plugin"),
    note = "comes with Docker Desktop; nothing to install separately",
    site = "https://docs.docker.com/compose/")

  val systemctl: Tool = Tool(
    name = "systemctl",
    probe = Vector("systemctl", "--version"),
    why = "the host target installs services as systemd units",
    note = "systemd is the init system; this target needs a Linux host that runs it",
    site = "https://www.freedesktop.org/wiki/Software/systemd/")

  val java: Tool = Tool(
    name = "java",
    probe = Vector("java", "-version"),
    atLeast = Some("17"),
    why = "a service built from a module runs on a JRE",
    install = Map(
      Manager.Brew -> "brew install openjdk@21",
      Manager.Apt -> "sudo apt-get install -y openjdk-21-jre-headless",
      Manager.Dnf -> "sudo dnf install -y java-21-openjdk-headless",
      Manager.Apk -> "sudo apk add openjdk21-jre-headless",
      Manager.Pacman -> "sudo pacman -S --noconfirm jre-openjdk-headless",
      Manager.Winget -> "winget install Microsoft.OpenJDK.21"),
    site = "https://adoptium.net/")

  val openssl: Tool = Tool(
    name = "openssl",
    probe = Vector("openssl", "version"),
    why = "certificates are built with it (okay-tls, okay-acme)",
    install = Map(
      Manager.Brew -> "brew install openssl",
      Manager.Apt -> "sudo apt-get install -y openssl",
      Manager.Dnf -> "sudo dnf install -y openssl",
      Manager.Apk -> "sudo apk add openssl",
      Manager.Pacman -> "sudo pacman -S --noconfirm openssl"),
    site = "https://www.openssl.org/")

  val sops: Tool = Tool(
    name = "sops",
    probe = Vector("sops", "--version"),
    why = "a secret is a sops: reference",
    install = Map(
      Manager.Brew -> "brew install sops",
      Manager.Apt -> "sudo apt-get install -y sops",
      Manager.Dnf -> "sudo dnf install -y sops",
      Manager.Pacman -> "sudo pacman -S --noconfirm sops"),
    site = "https://getsops.io/")

  val age: Tool = Tool(
    name = "age",
    probe = Vector("age", "--version"),
    why = "sops decrypts with an age key",
    install = Map(
      Manager.Brew -> "brew install age",
      Manager.Apt -> "sudo apt-get install -y age",
      Manager.Dnf -> "sudo dnf install -y age",
      Manager.Pacman -> "sudo pacman -S --noconfirm age"),
    site = "https://age-encryption.org/")

  val kubectl: Tool = Tool(
    name = "kubectl",
    probe = Vector("kubectl", "version", "--client"),
    why = "the cluster target applies manifests",
    install = Map(
      Manager.Brew -> "brew install kubectl",
      Manager.Apt -> "sudo apt-get install -y kubectl",
      Manager.Winget -> "winget install Kubernetes.kubectl"),
    ready = Some(Ready(
      Vector("kubectl", "config", "current-context"),
      "kubectl has no current context",
      "point it at a cluster: `kubectl config use-context <name>`")),
    site = "https://kubernetes.io/docs/tasks/tools/")

  val helm: Tool = Tool(
    name = "helm",
    probe = Vector("helm", "version", "--short"),
    why = "the cluster target installs a chart",
    install = Map(
      Manager.Brew -> "brew install helm",
      Manager.Apt -> "sudo apt-get install -y helm",
      Manager.Winget -> "winget install Helm.Helm"),
    site = "https://helm.sh/docs/intro/install/")

  val flyctl: Tool = Tool(
    name = "flyctl",
    probe = Vector("flyctl", "version"),
    why = "the fly target deploys the application",
    install = Map(Manager.Brew -> "brew install flyctl"),
    ready = Some(Ready(
      Vector("flyctl", "auth", "whoami"),
      "nobody is logged in to fly.io",
      "run `flyctl auth login`")),
    site = "https://fly.io/docs/flyctl/install/")

  val aws: Tool = Tool(
    name = "aws",
    probe = Vector("aws", "--version"),
    why = "a secret is an aws-sm: reference",
    install = Map(
      Manager.Brew -> "brew install awscli",
      Manager.Apt -> "sudo apt-get install -y awscli"),
    ready = Some(Ready(
      Vector("aws", "sts", "get-caller-identity"),
      "the aws CLI has no working credentials",
      "run `aws configure`, or set AWS_PROFILE")),
    site = "https://docs.aws.amazon.com/cli/")

  val gcloud: Tool = Tool(
    name = "gcloud",
    probe = Vector("gcloud", "--version"),
    why = "a secret is a gcp-sm: reference",
    install = Map(Manager.Brew -> "brew install --cask google-cloud-sdk"),
    ready = Some(Ready(
      Vector("gcloud", "config", "get-value", "project"),
      "gcloud has no active project",
      "run `gcloud init`, or `gcloud config set project <id>`")),
    site = "https://cloud.google.com/sdk/docs/install")

  val az: Tool = Tool(
    name = "az",
    probe = Vector("az", "version"),
    why = "a secret is an azure-kv: reference",
    install = Map(
      Manager.Brew -> "brew install azure-cli",
      Manager.Apt -> "sudo apt-get install -y azure-cli"),
    ready = Some(Ready(
      Vector("az", "account", "show"),
      "nobody is logged in to Azure",
      "run `az login`")),
    site = "https://learn.microsoft.com/cli/azure/")

  val terraform: Tool = Tool(
    name = "terraform",
    probe = Vector("terraform", "version"),
    why = "a cloud target applies infrastructure",
    install = Map(Manager.Brew -> "brew install terraform"),
    site = "https://developer.hashicorp.com/terraform/install")

  /** the ONE catalogue a target's `requires` is resolved against; a
   * name not in here is `Presence.Unknown`, so a target cannot
   * require something the doctor would quietly skip */
  val all: Vector[Tool] = Vector(
    docker, compose, systemctl, java, openssl, sops, age,
    kubectl, helm, flyctl, aws, gcloud, az, terraform)

  def byName(n: String): Option[Tool] = all.find(_.name == n)

  /** the tool a secret scheme needs, if it needs one: `env:` and
   * `file:` need nothing, which is most of them */
  def forScheme(scheme: String): Option[Tool] = scheme match
    case "sops" => Some(sops)
    case "aws-sm" => Some(aws)
    case "gcp-sm" => Some(gcloud)
    case "azure-kv" => Some(az)
    case _ => None

  /** the package manager THIS machine has, chosen once so the report
   * shows one command rather than a list to read past */
  def manager(): Manager =
    val os = System.getProperty("os.name", "").toLowerCase
    val candidates =
      if os.contains("mac") then Vector(Manager.Brew -> "brew")
      else if os.contains("win") then Vector(Manager.Winget -> "winget")
      else Vector(
        Manager.Apt -> "apt-get", Manager.Dnf -> "dnf",
        Manager.Apk -> "apk", Manager.Pacman -> "pacman", Manager.Brew -> "brew")
    candidates.find((_, bin) => onPath(bin)).map(_._1).getOrElse(Manager.Manual)

  def onPath(bin: String): Boolean =
    val sep = System.getProperty("path.separator", ":")
    val exts = if System.getProperty("os.name", "").toLowerCase.contains("win") then Vector(".exe", ".cmd", "") else Vector("")
    Option(System.getenv("PATH")).getOrElse("").split(_root_.java.util.regex.Pattern.quote(sep)).exists { d =>
      d.nonEmpty && exts.exists(e => _root_.java.nio.file.Files.isExecutable(_root_.java.nio.file.Path.of(d, bin + e)))
    }
