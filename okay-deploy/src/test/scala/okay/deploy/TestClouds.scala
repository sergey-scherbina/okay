package okay.deploy

import okay.conf.Secret

/**
 * specs/deployment.md, stage 3 finished: what `gcp` and `azure` say,
 * as arithmetic over the value.
 *
 * The three clouds are here TOGETHER on purpose. Each test that names
 * all three is a test of the model rather than of a renderer: the
 * same `Need` reaching three different answers, one of which is a
 * refusal, is the whole claim the model makes.
 */
class TestClouds extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages"),
    secrets = Vector(Secret("env:ADMIN_TOKEN"), Secret("file:/run/secrets/other")),
    needs = Vector(
      Need.Port(8080),
      Need.Database(Engine.Postgres, "16", "shop"),
      Need.Cache(Engine.Redis, "7"),
      Need.Dns("shop.example.com"), Need.Tls(TlsMode.Acme),
      Need.Region("europe-west1")),
    scale = Scale(2))

  private val shop = Deployment("shop", Vector(web))

  private def files(t: Target, d: Deployment = shop): Map[String, String] =
    t.render(d).getOrElse(fail(s"${t.name} refused: ${t.render(d)}")).toMap

  private def withVolume: Deployment =
    shop.copy(services = Vector(web.copy(needs = web.needs :+ Need.Volume("/app/data", size = "3Gi"))))

  // ---- the disagreement that is the point ----------------------------

  test("A Need.Volume reaches three different answers, and one of them is a REFUSAL") {
    // aws: EFS, a real filesystem
    assert(files(Aws, withVolume)("web.tf").contains("aws_efs_file_system"))
    // azure: an Azure Files share, also a real filesystem
    assert(files(Azure, withVolume)("web.tf").contains("azurerm_storage_share"))
    // gcp: refused, because the thing Cloud Run can mount is not one
    Gcp.render(withVolume) match
      case Right(_) => fail("a gcsfuse mount was rendered for a durable directory")
      case Left(why) =>
        assert(why.contains("web"), why)
        assert(why.contains("gcsfuse"), why)
        assert(why.contains("atomic rename"), why)
        // and it names the two answers that work
        assert(why.contains("cluster"), why)
        assert(why.contains("database"), why)
  }

  test("every cloud refuses without a region, and each names its own word for one") {
    val noRegion = shop.copy(services = Vector(web.copy(needs =
      web.needs.filterNot(_.isInstanceOf[Need.Region]))))
    assert(Aws.render(noRegion).left.exists(_.contains("region")))
    assert(Gcp.render(noRegion).left.exists(_.contains("region")))
    // Azure calls it a location, and the message says location
    assert(Azure.render(noRegion).left.exists(_.contains("location")), Azure.render(noRegion).toString)
  }

  test("no cloud makes a secret a Terraform resource: apply would write the value into the state") {
    for t <- Vector(Aws, Gcp, Azure); (path, content) <- files(t) if path.endsWith(".tf") do
      assert(!content.contains("resource \"aws_secretsmanager_secret\""), s"${t.name}/$path")
      assert(!content.contains("resource \"google_secret_manager_secret\" "), s"${t.name}/$path")
      assert(!content.contains("resource \"azurerm_key_vault_secret\""), s"${t.name}/$path")
    // each READS one instead
    assert(files(Gcp)("web.tf").contains("data \"google_secret_manager_secret\" \"ADMIN_TOKEN\""))
    assert(files(Azure)("web.tf").contains("data \"azurerm_key_vault_secret\" \"ADMIN_TOKEN\""))
    // and each setup.sh creates it, saying why it is not a .tf
    for t <- Vector(Aws, Gcp, Azure) do
      assert(files(t)("setup.sh").contains("state file"), t.name)
  }

  test("no rendering on any cloud carries a secret value, and a file: reference is wired nowhere") {
    for t <- Vector(Aws, Gcp, Azure); (path, content) <- files(t) do
      assert(!content.contains("/run/secrets/other"), s"${t.name}/$path wired a file: reference")
      for bad <- Vector("hunter2", "local-only") do assert(!content.contains(bad), s"${t.name}/$path")
  }

  // ---- gcp -----------------------------------------------------------

  test("gcp is Cloud Run: public by a flag, and its own name has a certificate already") {
    val w = files(Gcp)("web.tf")
    assert(w.contains("resource \"google_cloud_run_v2_service\" \"web\""), w)
    assert(w.contains("ingress = \"INGRESS_TRAFFIC_ALL\""), w)
    assert(w.contains("google_cloud_run_v2_service_iam_member"), w)
    assert(w.contains("output \"web_url\""), w)
    assert(w.contains("managed certificate from the moment it exists"), w)
  }

  test("gcp's project has NO default, because it is a billing and IAM boundary") {
    val m = files(Gcp)("main.tf")
    assert(m.contains("variable \"project\""), m)
    val block = m.slice(m.indexOf("variable \"project\""), m.indexOf("variable \"region\""))
    // the ARGUMENT, not the word: the description says "no default"
    assert(!block.linesIterator.exists(_.trim.startsWith("default")), block)
    assert(m.contains("THE PROJECT IS NOT OURS"), m)
  }

  test("gcp's database is private, and its URL is built from the private address") {
    val w = files(Gcp)("web.tf")
    assert(w.contains("resource \"google_sql_database_instance\" \"web_db\""), w)
    assert(w.contains("ipv4_enabled    = false"), w)
    assert(w.contains("database_version = \"POSTGRES_16\""), w)
    assert(w.contains("jdbc:postgresql://${google_sql_database_instance.web_db.private_ip_address}"), w)
  }

  test("a custom domain says out loud that a person must verify it first") {
    val w = files(Gcp)("web.tf")
    assert(w.contains("google_cloud_run_domain_mapping"), w)
    assert(w.contains("gcloud domains verify shop.example.com"), w)
    assert(files(Azure)("web.tf").contains("NEEDS VERIFICATION FIRST"), files(Azure)("web.tf"))
  }

  // ---- azure ---------------------------------------------------------

  test("azure is Container Apps, and the resource group is the one thing it owns") {
    val m = files(Azure)("main.tf")
    assert(m.contains("resource \"azurerm_resource_group\" \"this\""), m)
    assert(m.contains("azurerm_container_app_environment"), m)
    assert(m.contains("THE RESOURCE GROUP IS OURS AND NOTHING ELSE IS"), m)
    // the vault is READ, and named by a variable: it is globally unique
    assert(m.contains("data \"azurerm_key_vault\" \"this\""), m)
    assert(m.contains("variable \"vault_name\""), m)
  }

  test("azure's volume is a REAL filesystem, and the file says why that matters") {
    val m = files(Azure, withVolume)("main.tf")
    assert(m.contains("azurerm_storage_account"), m)
    assert(m.contains("atomic rename and locking"), m)
    val w = files(Azure, withVolume)("web.tf")
    assert(w.contains("azurerm_container_app_environment_storage"), w)
    assert(w.contains("storage_type = \"AzureFile\""), w)
    assert(w.contains("path = \"/app/data\""), w)
    // and no storage account is rendered when nothing asks for one
    assert(!files(Azure)("main.tf").contains("azurerm_storage_account"), files(Azure)("main.tf"))
  }

  test("azure's secrets travel by the app's own secret block, never as a literal") {
    val w = files(Azure)("web.tf")
    assert(w.contains("value = data.azurerm_key_vault_secret.ADMIN_TOKEN.value"), w)
    assert(w.contains("secret_name = \"admin-token\""), w)
    // a Key Vault name is letters, digits and dashes: nothing else
    assertEquals(Azure.kvName("shop-web-ADMIN_TOKEN"), "shop-web-ADMIN-TOKEN")
    assertEquals(Azure.kvName("_leading/and.trailing_"), "leading-and-trailing")
  }

  // ---- the shape -----------------------------------------------------

  test("all three clouds name their tools, apply through terraform, and are in Targets.all") {
    assertEquals(Gcp.requires(shop), Vector("terraform", "gcloud"))
    assertEquals(Azure.requires(shop), Vector("terraform", "az"))
    for n <- Vector("aws", "gcp", "azure") do assertEquals(Targets.byName(n).map(_.name), Some(n))
    assertEquals(Gcp.up(java.nio.file.Path.of("/x")).head, "terraform")
  }
