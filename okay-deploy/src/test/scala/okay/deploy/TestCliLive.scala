package okay.deploy

import java.nio.file.{Files, Path}

/**
 * The one test here that runs the real thing: `okay deploy up` on a
 * real docker, from an artifacts directory that has nothing else in
 * it — no repository, no sbt, no source.
 *
 * Live-tagged (`--include-tags=Live`) because it needs a running
 * docker daemon, and skipped where there is none: the doctor's own
 * check decides, which also makes this the one place where the
 * doctor's answer is compared against reality rather than a fixture.
 *
 * The image is a stock nginx, deliberately: this proves the CLI's
 * own path (render, check, apply, stop) and nothing about any
 * application.
 */
class TestCliLive extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val idler = Deployment("okaylive", Vector(Service(
    name = "idler",
    run = Run.Image("nginx", "alpine"),
    settings = Settings.of("okay")("hold" -> "1"))))

  test("a rendered directory, a real docker, and `up` then `down` — the whole road") {
    assume(Doctor.probe(Tools.docker).ok, "no usable docker on this machine")

    val root = Files.createTempDirectory("okay-live")
    val dir = root.resolve(Deployment.dir(idler, "laptop"))
    try
      Deployment.write(idler, Targets.Laptop, root): Unit
      assert(Files.isRegularFile(dir.resolve("deployment.json")))

      val said = Vector.newBuilder[String]
      val complained = Vector.newBuilder[String]
      def cli(args: String*): Int = Cli.run(args.toVector, said += _, complained += _, dir)

      // the doctor agrees with the machine, which is the assumption
      // every other test in this module makes with a fixture
      assertEquals(cli("doctor"), Cli.Exit.ok, complained.result().mkString("\n"))

      try
        assertEquals(cli("up"), Cli.Exit.ok, complained.result().mkString("\n"))
        val ps = Shell.run(Vector("docker", "compose", "-f", dir.resolve("compose.yaml").toString, "ps"))
        assert(ps.ok, ps.text)
        assert(ps.text.contains("idler"), ps.text)
      finally
        assertEquals(cli("down"), Cli.Exit.ok, complained.result().mkString("\n"))

      // and the rendering it applied is still exactly the value
      assertEquals(cli("diff"), Cli.Exit.ok, said.result().mkString("\n"))
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)
  }
