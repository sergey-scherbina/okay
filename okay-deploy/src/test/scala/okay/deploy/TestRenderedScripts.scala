package okay.deploy

import okay.conf.Secret

import java.nio.file.{Files, Path}

/**
 * Every shell script every target renders, put in front of a real
 * shell (specs/deployment.md).
 *
 * It exists because `sh -n` found the same defect in TWO targets, one
 * of them already landed: an apostrophe in "the master password for
 * web's Postgres" opened a quote inside a `${VAR:?message}` that
 * nothing closed, so the shell swallowed the `}` and died at end of
 * file. The cluster target had shipped that in stage 1 and no test
 * had ever run its script.
 *
 * So this walks `Targets.all` rather than naming targets: a target
 * added later is covered the day it is written.
 *
 * Live, because `sh` is a subprocess.
 */
class TestRenderedScripts extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  /** everything at once: a secret, a database whose description holds
   * the character that started this, a cache, a volume, a domain */
  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages", "motd" -> """a "quoted" ' value"""),
    secrets = Vector(Secret("env:ADMIN_TOKEN"), Secret("file:/run/secrets/other")),
    needs = Vector(
      Need.Port(8080), Need.Volume("/app/data", size = "3Gi"),
      Need.Database(Engine.Postgres, "16.3", "shop"),
      Need.Cache(Engine.Redis, "7.1"),
      Need.Dns("shop.example.com"), Need.Tls(TlsMode.Acme),
      Need.Region("eu-central-1")),
    scale = Scale(2))

  private val worker = Service(
    name = "worker",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    needs = Vector(Need.Neighbour("web"), Need.Region("eu-central-1")))

  private val shop = Deployment("shop", Vector(web, worker))

  test("every .sh every target renders is one `sh` accepts") {
    val root = Files.createTempDirectory("okay-scripts")
    var checked = 0
    try
      for t <- Targets.all do
        t.render(shop) match
          // a target that refuses this deployment renders nothing,
          // which is its own answer and not this test's business
          case Left(_) => ()
          case Right(_) =>
            Deployment.write(shop, t, root): Unit
            val dir = root.resolve(Deployment.dir(shop, t.name))
            val scripts = Files.walk(dir).toArray.map(_.asInstanceOf[Path])
              .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".sh")).toVector
            for sh <- scripts do
              checked += 1
              // -n parses without running: nothing is created, no
              // account is touched, and a broken quote still fails
              val out = Shell.run(Vector("sh", "-n", sh.toString))
              assert(out.ok, s"${t.name}/${dir.relativize(sh)} is not valid sh:\n${out.text}")
      assert(checked >= 5, s"only $checked scripts were checked; a target stopped rendering one")
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)
  }
