package okay.deploy

import okay.codec.Json
import okay.conf.Secret

/**
 * specs/cluster-pool.md, stage 3: nomad, yarn, slurm, swarm and batch,
 * as arithmetic over the value — the same bar specs/deployment.md's
 * own stages set. `TestManagersLive` is the real-parser half.
 */
class TestManagers extends munit.FunSuite:

  private val pooled = Service("pool", Run.Image("okay/pool", "1"),
    settings = Settings.of("okay")("pages" -> "/app/pages"),
    needs = Vector(Need.Port(7100), Need.Peers, Need.Region("eu-central-1")),
    scale = Scale(3))

  private val d = Deployment("shop", Vector(pooled))

  private def files(t: Target, dep: Deployment = d): Map[String, String] =
    t.render(dep).getOrElse(fail(s"${t.name} refused: ${t.render(dep)}")).toMap

  // ---- nomad ----------------------------------------------------------

  test("nomad: a job.json this build can read back, one group per service") {
    val text = files(Managers.Nomad)("job.json")
    Json.parse(text) match
      case Json.JObj(top) =>
        val job = top.collectFirst { case ("Job", Json.JObj(j)) => j }.getOrElse(fail("no Job"))
        assertEquals(job.collectFirst { case ("ID", Json.JStr(s)) => s }, Some("shop"))
        val groups = job.collectFirst { case ("TaskGroups", Json.JArr(g)) => g }.getOrElse(fail("no groups"))
        assertEquals(groups.length, 1)
        val Json.JObj(g0) = groups.head: @unchecked
        assertEquals(g0.collectFirst { case ("Count", Json.JNum(n)) => n.toInt }, Some(3))
        val svcs = g0.collectFirst { case ("Services", Json.JArr(s)) => s }.getOrElse(fail("no Services"))
        val Json.JObj(svc0) = svcs.head: @unchecked
        // Nomad's OWN discovery -- no Consul required
        assertEquals(svc0.collectFirst { case ("Provider", Json.JStr(p)) => p }, Some("nomad"))
      case other => fail(s"not an object: $other")
  }

  test("nomad: OKAY_POOL_SERVICE names Nomad's own service DNS") {
    val text = files(Managers.Nomad)("job.json")
    assert(text.contains("\"OKAY_POOL_SERVICE\":\"pool.service.nomad\""), text)
  }

  test("no Need.Peers, no Services stanza and no OKAY_POOL_SERVICE") {
    val plain = Deployment("shop", Vector(Service("web", Run.Image("nginx", "alpine"))))
    val text = files(Managers.Nomad, plain)("job.json")
    assert(!text.contains("OKAY_POOL_SERVICE"), text)
    assert(!text.contains("\"Services\""), text)
  }

  // ---- yarn -------------------------------------------------------------

  test("yarn: a Yarnfile this build can read back") {
    val text = files(Managers.Yarn)("Yarnfile")
    Json.parse(text) match
      case Json.JObj(fs) =>
        assertEquals(fs.collectFirst { case ("name", Json.JStr(s)) => s }, Some("shop"))
        val comps = fs.collectFirst { case ("components", Json.JArr(c)) => c }.getOrElse(fail("no components"))
        val Json.JObj(c0) = comps.head: @unchecked
        assertEquals(c0.collectFirst { case ("number_of_containers", Json.JNum(n)) => n.toInt }, Some(3))
      case other => fail(s"not an object: $other")
  }

  test("yarn: OKAY_POOL_SERVICE is a TEMPLATE, not a guessed DNS zone") {
    val text = files(Managers.Yarn)("Yarnfile")
    assert(text.contains("your cluster's DNS zone"), text)
  }

  // ---- slurm --------------------------------------------------------

  test("slurm: one sbatch script, --ntasks matches the replica count") {
    val text = files(Managers.Slurm)("job.sbatch")
    assert(text.contains("#SBATCH --ntasks=3"), text)
    assert(text.contains("#!/bin/sh"), text)
  }

  test("slurm: the peer list comes from scontrol at RUN time, never guessed") {
    val text = files(Managers.Slurm)("job.sbatch")
    assert(text.contains("scontrol show hostnames \"$SLURM_JOB_NODELIST\""), text)
    assert(text.contains("OKAY_POOL_PEERS="), text)
  }

  test("slurm REFUSES more than one Need.Peers service in one deployment") {
    val two = Deployment("shop", Vector(pooled, pooled.copy(name = "pool2")))
    Managers.Slurm.render(two) match
      case Right(_) => fail("two pool services rendered as one slurm job")
      case Left(why) => assert(why.contains("pool") && why.contains("pool2"), why)
  }

  // ---- swarm ----------------------------------------------------------

  test("swarm: replicated mode, and tasks.<service> for OKAY_POOL_SERVICE") {
    val text = files(Managers.Swarm)("compose.yaml")
    assert(text.contains("    deploy:\n      mode: replicated\n      replicas: 3\n"), text)
    assert(text.contains("OKAY_POOL_SERVICE: \"tasks.pool\""), text)
  }

  // ---- batch ----------------------------------------------------------

  test("batch: a multinode job definition, one node per replica") {
    val text = files(Managers.Batch)("main.tf")
    assert(text.contains("type = \"multinode\""), text)
    assert(text.contains("num_nodes = 3"), text)
    assert(text.contains("target_nodes = \"0:2\""), text)
  }

  test("batch NEVER renders OKAY_POOL_PEERS -- it does not have the address to give") {
    val text = files(Managers.Batch)("main.tf")
    assert(!text.contains("OKAY_POOL_PEERS"), text)
    assert(!text.contains("OKAY_POOL_SERVICE"), text)
  }

  test("batch REFUSES without a region") {
    val noRegion = d.copy(services = Vector(pooled.copy(needs = pooled.needs.filterNot(_.isInstanceOf[Need.Region]))))
    Managers.Batch.render(noRegion) match
      case Right(_) => fail("a region was invented")
      case Left(why) => assert(why.contains("region"), why)
  }

  // ---- the refusal all five share -------------------------------------

  test("all five refuse a database, a cache, and a secret by name") {
    val withDb = d.copy(services = Vector(pooled.copy(needs = pooled.needs :+ Need.Database(Engine.Postgres, "16", "x"))))
    val withCache = d.copy(services = Vector(pooled.copy(needs = pooled.needs :+ Need.Cache(Engine.Redis, "7"))))
    val withSecret = d.copy(services = Vector(pooled.copy(secrets = Vector(Secret("env:TOKEN")))))
    for t <- Vector(Managers.Nomad, Managers.Yarn, Managers.Slurm, Managers.Swarm, Managers.Batch) do
      t.render(withDb) match
        case Right(_) => fail(s"${t.name} rendered a database it does not run")
        case Left(why) => assert(why.contains("Postgres"), s"${t.name}: $why")
      t.render(withCache) match
        case Right(_) => fail(s"${t.name} rendered a cache it does not run")
        case Left(why) => assert(why.contains("Redis"), s"${t.name}: $why")
      t.render(withSecret) match
        case Right(_) => fail(s"${t.name} rendered a secret with no wiring")
        case Left(why) => assert(why.contains("secret"), s"${t.name}: $why")
  }

  test("no rendering, on any of the five, carries a secret VALUE") {
    for t <- Vector(Managers.Nomad, Managers.Yarn, Managers.Slurm, Managers.Swarm, Managers.Batch) do
      for (path, content) <- files(t) do
        assert(!content.contains("hunter2"), s"${t.name}/$path")
  }

  // ---- the shape -----------------------------------------------------

  test("all five name their tools and are in Targets.all") {
    for n <- Vector("nomad", "yarn", "slurm", "swarm", "batch") do
      assertEquals(Targets.byName(n).map(_.name), Some(n))
    assertEquals(Managers.Nomad.requires(d), Vector("nomad"))
    assertEquals(Managers.Slurm.requires(d), Vector("sbatch"))
  }
