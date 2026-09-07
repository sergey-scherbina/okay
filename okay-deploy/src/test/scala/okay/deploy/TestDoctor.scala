package okay.deploy

import okay.conf.Secret

/**
 * specs/deployment.md, "The clean machine": the report an operator
 * reads on a machine where nothing is installed.
 *
 * Every row here is produced with an INJECTED prober, which is the
 * point: the states that matter most — a stopped docker daemon, a
 * kubectl with no context, a version that is behind — are the ones a
 * real machine will not reproduce on demand.
 */
class TestDoctor extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    secrets = Vector(Secret("env:TOKEN"), Secret("sops:secrets.yaml#pg"), Secret("file:/run/secrets/x")),
    needs = Vector(Need.Port(8080), Need.Tls(TlsMode.SelfSigned), Need.Database(Engine.Postgres, "16", "shop")))

  private val shop = Deployment("shop", Vector(web))

  test("every tool the doctor asks about carries WHY the deployment needs it, and the why names the thing that asked") {
    val want = Doctor.wanted(shop, Targets.Laptop)
    val by = want.map(w => w.name -> w.why).toMap

    assert(by.contains("docker"), by.keys.toString)
    assert(by("docker").exists(_.contains("`laptop` target")), by("docker").toString)
    // a sops: reference is the reason sops is needed -- an operator
    // can delete the need instead of installing the tool
    assert(by("sops").exists(_.contains("sops:secrets.yaml#pg")), by("sops").toString)
    assert(by.contains("age"), by.keys.toString)
    assert(by("openssl").exists(_.contains("web")), by("openssl").toString)
    // env: and file: need nothing, and that is most of them
    assertEquals(want.count(_.name == "aws"), 0)
  }

  test("a missing tool names why, the install command for THIS machine, and the docs") {
    val r = Doctor.check(shop, Targets.Laptop, _ => Presence.Missing).copy(manager = Manager.Brew)
    val t = r.table
    assert(!r.ready)
    assert(t.contains("MISSING"), t)
    assert(t.contains("brew install --cask docker"), t)
    assert(t.contains("https://docs.docker.com/get-started/"), t)
    // the last two lines say what happened and what to do next
    assert(t.contains("Nothing has been applied."), t)
    assert(t.contains("--install"), t)
  }

  test("the install line shown is the detected manager's, not a list of five to read past") {
    val brew = Doctor.check(shop, Targets.Laptop, _ => Presence.Missing).copy(manager = Manager.Brew).table
    val apt = Doctor.check(shop, Targets.Laptop, _ => Presence.Missing).copy(manager = Manager.Apt).table
    assert(brew.contains("brew install --cask docker") && !brew.contains("apt-get install -y docker.io"), brew)
    assert(apt.contains("sudo apt-get install -y docker.io") && !apt.contains("brew install --cask docker"), apt)
  }

  test("a tool with nothing to run for this manager says so in words rather than showing a command") {
    val r = Doctor.check(shop, Targets.Laptop, t => if t.name == "docker compose" then Presence.Missing else Presence.Ok("1"))
      .copy(manager = Manager.Brew)
    val t = r.table
    assert(t.contains("comes with Docker Desktop"), t)
    assert(!t.contains("brew install docker compose"), t)
  }

  test("installed and still unusable is NOT ready, and the row carries the fix") {
    val stopped = Presence.NotReady("the Docker daemon is not running", "start Docker Desktop, or `sudo systemctl start docker`")
    val r = Doctor.check(shop, Targets.Laptop, t => if t.name == "docker" then stopped else Presence.Ok("1.0"))
    assert(!r.ready)
    val t = r.table
    assert(t.contains("NOT READY"), t)
    assert(t.contains("the Docker daemon is not running"), t)
    assert(t.contains("fix:      start Docker Desktop"), t)
  }

  test("a version behind the minimum is TOO OLD, with both numbers -- and we never pin, only report") {
    val r = Doctor.check(shop, Targets.Laptop, t =>
      if t.name == "docker" then Presence.TooOld("19.03", "20.10") else Presence.Ok("1.0"))
    assert(!r.ready)
    assert(r.table.contains("19.03 is installed and this deployment needs 20.10 or newer"), r.table)
  }

  test("a name the catalogue does not know is a finding, not a silent pass") {
    object Odd extends Target:
      def name = "odd"
      def render(d: Deployment) = Right(Vector("odd.txt" -> "x"))
      def requires(d: Deployment) = Vector("nosuchtool")
      def up(dir: java.nio.file.Path) = Vector("true")
      def down(dir: java.nio.file.Path) = Vector("true")
    val r = Doctor.check(shop, Odd, _ => Presence.Ok("1"))
    assert(!r.ready)
    assertEquals(r.rows.find(_.name == "nosuchtool").map(_.presence), Some(Presence.Unknown))
    assert(r.table.contains("okay-deploy's catalogue"), r.table)
  }

  test("--json carries the same fields as the table, so a pipeline never parses one") {
    val r = Doctor.check(shop, Targets.Laptop, t => if t.name == "docker" then Presence.Missing else Presence.Ok("3.5.0"))
      .copy(manager = Manager.Brew)
    val j = r.json
    assert(j.contains("\"ready\":false"), j)
    assert(j.contains("\"state\":\"missing\""), j)
    assert(j.contains("\"install\":\"brew install --cask docker\""), j)
    assert(j.contains("\"version\":\"3.5.0\""), j)
    // and it parses
    okay.codec.Json.parse(j) match
      case okay.codec.Json.JObj(fs) => assert(fs.exists(_._1 == "tools"), j)
      case other => fail(s"not an object: $other")
  }

  test("--install prints every command BEFORE running it, and re-checks afterwards") {
    val r = Doctor.check(shop, Targets.Laptop, _ => Presence.Missing).copy(manager = Manager.Brew)
    val said = Vector.newBuilder[String]
    val ran = Vector.newBuilder[Vector[String]]
    val done = Doctor.install(r, said += _, cmd => { ran += cmd; Shell.Out(0, "") })
    val log = said.result()
    val cmds = ran.result()
    assert(log.contains("$ brew install --cask docker"), log.toString)
    assert(cmds.exists(_.mkString(" ").contains("brew install --cask docker")), cmds.toString)
    // printed before it ran: the echo of a command precedes its result
    assert(log.indexOf("$ brew install --cask docker") < log.indexWhere(_ == "docker: installed"), log.toString)
    assert(done.contains("docker"), done.toString)
  }

  test("a failed install names the command, the exit code and the output -- no silent failure") {
    val r = Doctor.check(shop, Targets.Laptop, _ => Presence.Missing).copy(manager = Manager.Brew)
    val said = Vector.newBuilder[String]
    Doctor.install(r, said += _, _ => Shell.Out(1, "")): Unit
    val log = said.result().mkString("\n")
    assert(log.contains("exit 1"), log)
    assert(log.contains("brew install --cask docker"), log)
    assert(log.contains("printed nothing at all"), log)
  }

  test("we never pipe a downloaded script into a shell, and no entry in the catalogue asks us to") {
    assert(Doctor.piped("curl -fsSL https://example.com/i.sh | sh"))
    assert(Doctor.piped("wget -qO- https://example.com/i.sh | bash"))
    assert(!Doctor.piped("brew install sops"))
    for t <- Tools.all; (m, cmd) <- t.install do
      assert(!Doctor.piped(cmd), s"${t.name} on $m: $cmd")
      assert(!cmd.contains("=="), s"${t.name} on $m pins a version, and we never do: $cmd")
  }

  test("a refused install is reported and not run") {
    val curl = Tools.docker.copy(install = Map(Manager.Brew -> "curl -fsSL https://x/i.sh | sh"))
    val r = Doctor.Report("laptop", "os", "arch", Manager.Brew,
      Vector(Doctor.Row("docker", Vector("why"), Some(curl), Presence.Missing)))
    val said = Vector.newBuilder[String]
    var ranAnything = false
    val done = Doctor.install(r, said += _, _ => { ranAnything = true; Shell.Out(0, "") })
    assert(!ranAnything)
    assert(done.isEmpty)
    assert(said.result().mkString.contains("never pipes a downloaded script"), said.result().toString)
  }

  test("version comparison is numeric per segment: 1.2.10 is newer than 1.2.9") {
    assert(Tool.atLeast("1.2.10", "1.2.9"))
    assert(Tool.atLeast("20.10", "20.10"))
    assert(!Tool.atLeast("19.03", "20.10"))
    assert(Tool.atLeast("21", "17"))
    assert(!Tool.atLeast("11.0.2", "17"))
  }

  test("the default version reader finds what a real --version prints") {
    assertEquals(Tool.firstVersion("Docker version 27.3.1, build ce12230"), Some("27.3.1"))
    assertEquals(Tool.firstVersion("openjdk version \"21.0.4\" 2024-07-16"), Some("21.0.4"))
    assertEquals(Tool.firstVersion("OpenSSL 3.5.0 8 Apr 2025"), Some("3.5.0"))
    assertEquals(Tool.firstVersion("nothing here"), None)
  }

  test("probing a binary that is not there is Missing, not a crash") {
    val ghost = Tool(name = "nosuchbinary", probe = Vector("nosuchbinary-okay-deploy", "--version"), why = "a test")
    assertEquals(Doctor.probe(ghost), Presence.Missing)
  }

  test("probing something real answers Ok with its version") {
    // `sh` is on every machine this suite runs on; the probe is a
    // command, not a package -- and this is the one test here that
    // touches the machine at all
    val sh = Tool(name = "sh", probe = Vector("sh", "-c", "echo 1.0.0"), why = "a test")
    assertEquals(Doctor.probe(sh), Presence.Ok("1.0.0"))
  }

  test("a second probe that fails turns Ok into NotReady with its own fix") {
    val t = Tool(
      name = "sh", probe = Vector("sh", "-c", "echo 1.0.0"), why = "a test",
      ready = Some(Ready(Vector("sh", "-c", "exit 3"), "it is not logged in", "run `x login`")))
    assertEquals(Doctor.probe(t), Presence.NotReady("it is not logged in", "run `x login`"))
  }

  test("a command that exits non-zero and prints NOTHING still produces a sentence") {
    val out = Shell.run(Vector("sh", "-c", "exit 7"))
    assertEquals(out.code, 7)
    val m = Shell.failure(Vector("sh", "-c", "exit 7"), out, "the thing")
    assert(m.contains("the thing failed (exit 7)"), m)
    assert(m.contains("command: sh -c \"exit 7\""), m)
    assert(m.contains("none — the command printed nothing at all"), m)
  }

  test("a command that cannot be started is 127 with the reason, the shape a shell gives") {
    val out = Shell.run(Vector("nosuchbinary-okay-deploy"))
    assertEquals(out.code, 127)
    assert(out.text.contains("nosuchbinary-okay-deploy"), out.text)
  }
