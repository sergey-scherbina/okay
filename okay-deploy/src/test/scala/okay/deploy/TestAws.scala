package okay.deploy

import okay.conf.Secret

/**
 * specs/deployment.md, stage 3: the AWS target's mapping, as
 * arithmetic over the value.
 *
 * What terraform itself thinks of the rendering is
 * `TestAwsTerraform`, which is Live and runs the provider's own
 * schema over it.
 */
class TestAws extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages"),
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

  private def files(d: Deployment = shop): Map[String, String] =
    Aws.render(d).getOrElse(fail(s"refused: ${Aws.render(d)}")).toMap

  test("one file per service, plus the shared one and the script") {
    val f = files()
    assertEquals(f.keySet, Set("main.tf", "setup.sh", "web.tf", "worker.tf"))
  }

  test("A SECRET IS NOT A TERRAFORM RESOURCE, because apply would write its value into the state") {
    val f = files()
    for (path, content) <- f if path.endsWith(".tf") do
      assert(!content.contains("resource \"aws_secretsmanager_secret\""), s"$path creates a secret")
      assert(!content.contains("aws_secretsmanager_secret_version\" \"") ||
        content.contains("data \"aws_secretsmanager_secret_version\""), s"$path writes a secret VERSION")
    // it is read back instead, and the reason is in the file
    assert(f("web.tf").contains("data \"aws_secretsmanager_secret\" \"ADMIN_TOKEN\""), f("web.tf"))
    assert(f("web.tf").contains("in the state file, and a secret") || f("web.tf").contains("plaintext"), f("web.tf"))
    // the task definition gets the ARN, never a value
    assert(f("web.tf").contains("valueFrom = data.aws_secretsmanager_secret.ADMIN_TOKEN.arn"), f("web.tf"))
    // and setup.sh creates it, saying why it is not a .tf
    assert(f("setup.sh").contains("aws secretsmanager create-secret"), f("setup.sh"))
    assert(f("setup.sh").contains("state file"), f("setup.sh"))
  }

  test("the execution role may read exactly the secrets this deployment names, and no others") {
    val m = files()("main.tf")
    assert(m.contains("secretsmanager:GetSecretValue"), m)
    assert(m.contains("data.aws_secretsmanager_secret.ADMIN_TOKEN.arn"), m)
    assert(!m.contains("\"*\""), m)
  }

  test("THE NETWORK IS NOT OURS: default VPC by data source, and nothing built") {
    val m = files()("main.tf")
    assert(m.contains("data \"aws_vpc\" \"default\""), m)
    assert(m.contains("data \"aws_subnets\" \"default\""), m)
    assert(m.contains("THE NETWORK IS NOT OURS"), m)
    for (path, content) <- files() if path.endsWith(".tf") do
      assert(!content.contains("resource \"aws_vpc\""), s"$path builds a VPC")
      assert(!content.contains("resource \"aws_subnet\""), s"$path builds a subnet")
      assert(!content.contains("resource \"aws_nat_gateway\""), s"$path builds a NAT gateway")
  }

  test("aws refuses without a region, the same as fly") {
    val d = shop.copy(services = Vector(web.copy(needs = web.needs.filterNot(_.isInstanceOf[Need.Region]))))
    Aws.render(d) match
      case Right(_) => fail("a region was invented")
      case Left(why) =>
        assert(why.contains("web"), why)
        assert(why.contains("Need.Region"), why)
  }

  test("a public port is an ALB with a target group and a listener; a private one is not") {
    val f = files()
    assert(f("web.tf").contains("resource \"aws_lb\" \"web\""), f("web.tf"))
    assert(f("web.tf").contains("resource \"aws_lb_target_group\" \"web\""), f("web.tf"))
    assert(f("web.tf").contains("path                = \"/readyz\""), f("web.tf"))
    // the worker has no port at all, so nothing faces the world
    assert(!f("worker.tf").contains("aws_lb"), f("worker.tf"))
  }

  test("a DNS name with Acme is ACM, a validation record, an alias and a redirect from 80") {
    val w = files()("web.tf")
    assert(w.contains("resource \"aws_acm_certificate\" \"web\""), w)
    assert(w.contains("validation_method         = \"DNS\""), w)
    assert(w.contains("resource \"aws_route53_record\" \"web_validation\""), w)
    assert(w.contains("resource \"aws_route53_record\" \"web_alias_0\""), w)
    assert(w.contains("status_code = \"HTTP_301\""), w)
    assert(w.contains("resource \"aws_lb_listener\" \"web_https\""), w)
    // the hosted zone is asked for, never guessed
    assert(files()("main.tf").contains("variable \"zone_id\""), files()("main.tf"))
  }

  test("without a DNS name there is no certificate, and the load balancer's address is an output") {
    val d = shop.copy(services = Vector(web.copy(needs =
      web.needs.filterNot(n => n.isInstanceOf[Need.Dns] || n.isInstanceOf[Need.Tls]))))
    val f = files(d)
    assert(!f("web.tf").contains("aws_acm_certificate"), f("web.tf"))
    assert(f("web.tf").contains("output \"web_url\""), f("web.tf"))
    assert(f("web.tf").contains("point your own DNS at it"), f("web.tf"))
    assert(!f("main.tf").contains("variable \"zone_id\""), f("main.tf"))
  }

  test("a volume is EFS with a mount target per subnet, and the task mounts it") {
    val w = files()("web.tf")
    assert(w.contains("resource \"aws_efs_file_system\" \"web_data\""), w)
    assert(w.contains("for_each = toset(data.aws_subnets.default.ids)"), w)
    assert(w.contains("encrypted      = true"), w)
    assert(w.contains("containerPath = \"/app/data\""), w)
  }

  test("a database is RDS whose password is READ, and the URL is an interpolation of its endpoint") {
    val w = files()("web.tf")
    assert(w.contains("resource \"aws_db_instance\" \"web_db\""), w)
    assert(w.contains("engine                 = \"postgres\""), w)
    assert(w.contains("password               = data.aws_secretsmanager_secret_version.web_db_password.secret_string"), w)
    assert(w.contains("""name = "DB_URL", value = "jdbc:postgresql://${aws_db_instance.web_db.endpoint}/shop""""), w)
    // the thing you cannot re-create from this file says so
    assert(w.contains("cannot\n  # re-create") || w.contains("re-create from this file"), w)
  }

  test("a cache is ElastiCache and its address reaches the task the same way") {
    val w = files()("web.tf")
    assert(w.contains("resource \"aws_elasticache_cluster\" \"web_cache\""), w)
    assert(w.contains("aws_elasticache_cluster.web_cache.cache_nodes[0].address"), w)
  }

  test("no rendering carries a secret VALUE, and a file: reference is not wired here at all") {
    for (path, content) <- files() do
      assert(!content.contains("/run/secrets/other"), s"$path wired a file: reference")
      for bad <- Vector("hunter2", "local-only") do assert(!content.contains(bad), path)
  }

  test("the target names its tools, applies through terraform, and is in Targets.all") {
    assertEquals(Aws.requires(shop), Vector("terraform", "aws"))
    val dir = java.nio.file.Path.of("/srv/shop/deploy/aws")
    assertEquals(Aws.up(dir), Vector("terraform", "-chdir=/srv/shop/deploy/aws", "apply"))
    assertEquals(Targets.byName("aws").map(_.name), Some("aws"))
    assertEquals(Targets.all.length, 7)
  }
