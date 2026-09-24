package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * What a real tool thinks of nomad/yarn/slurm/swarm/batch's rendering
 * (specs/cluster-pool.md, stage 3) — the same bar every target in
 * this arc gates on: never a golden file. `Live`, per AGENTS.md, for
 * every suite that shells out; `nomad`/`yarn`/`sbatch` themselves are
 * not required to be installed (neither renderer needs them to check
 * it is well-formed — see `TestManagers` for the JSON round-trip),
 * only `sh`, `docker` and a `hashicorp/terraform` image, which the
 * cloud suites already depend on.
 */
class TestManagersLive extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val pooled = Service("pool", Run.Image("okay/pool", "1"),
    settings = Settings.of("okay")("pages" -> "/app/pages"),
    needs = Vector(Need.Port(7100), Need.Peers, Need.Region("eu-central-1")),
    scale = Scale(3))

  private val d = Deployment("shop", Vector(pooled))

  private def rendered(t: Target)(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("okay-mgr")
    try
      Deployment.write(d, t, root): Unit
      body(root.resolve(Deployment.dir(d, t.name)))
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  test("slurm: the script passes sh -n") {
    rendered(Managers.Slurm) { dir =>
      val out = Shell.run(Vector("sh", "-n", dir.resolve("job.sbatch").toString))
      assert(out.ok, out.text)
    }
  }

  test("swarm: docker compose config accepts the rendering") {
    assume(Doctor.probe(Tools.docker).ok, "no usable docker on this machine")
    rendered(Managers.Swarm) { dir =>
      val out = Shell.run(Vector("docker", "compose", "-f", dir.resolve("compose.yaml").toString, "config", "-q"))
      assert(out.ok, out.text)
    }
  }

  test("batch: terraform validate accepts the multinode job definition") {
    assume(Doctor.probe(Tools.docker).ok, "no usable docker on this machine")
    rendered(Managers.Batch) { dir =>
      def tf(args: String*): Shell.Out =
        Shell.run(Vector("docker", "run", "--rm", "-v", s"${dir.toString}:/w", "-w", "/w",
          "hashicorp/terraform:latest") ++ args.toVector :+ "-no-color")
      val init = tf("init", "-input=false")
      assume(init.ok || !init.text.contains("Failed to query available provider packages"),
        s"the provider registry is not reachable:\n${init.tail(4)}")
      assert(init.ok, s"terraform init rejected the rendering:\n${init.text}")
      val out = tf("validate")
      assert(out.ok, s"terraform validate rejected the rendering:\n${out.text}")
      assert(tf("fmt", "-check", "-diff").ok, "terraform fmt would reformat the batch rendering")
    }
  }

  test("Claim 1: no manager's name or client lives in okay-cluster's or okay-pool's main sources") {
    val root = Deploy.repoRoot()
    val names = Vector("kubernetes", "k8s", "kubectl", "helm", "nomad", "consul", "yarn",
      "hadoop", "slurm", "sbatch", "terraform", "com.amazonaws", "software.amazon.awssdk")
    // WORD boundaries, not substrings: a plain `contains("consul")` is
    // also true of "consult", and this test exists to catch a real
    // dependency, not an English word (the first run of it did exactly
    // that — okay.cluster.Flows.scala's own doc comment: "...should
    // say Merge or Shuffle and not consult a number...")
    val patterns = names.map(n => n -> raw"(?i)\b${scala.util.matching.Regex.quote(n)}\b".r)
    for module <- Vector("okay-cluster", "okay-pool") do
      val src = root.resolve(module).resolve("src").resolve("main")
      if Files.exists(src) then
        val walk = Files.walk(src)
        val hits =
          try
            walk.iterator().asScala.toVector
              .filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".java"))
              .flatMap { p =>
                val text = Files.readString(p)
                patterns.filter((_, re) => re.findFirstIn(text).isDefined).map((n, _) => s"$p: $n")
              }
          finally walk.close()
        assertEquals(hits, Vector.empty, s"$module names a cluster manager directly")
  }
