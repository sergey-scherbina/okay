package okay.deploy

import okay.conf.Secret

/**
 * specs/deployment.md, stage 1: the cluster target's mapping, as
 * arithmetic over the value.
 *
 * Everything here is a function of the `Deployment` and nothing else,
 * so it runs in the default suite. What helm itself thinks of the
 * rendering is `TestClusterHelm`, which is Live — the repository's
 * rule for every suite that shells out.
 */
class TestCluster extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("pages" -> "/app/pages", "port" -> "8080"),
    secrets = Vector(Secret("env:ADMIN_TOKEN"), Secret("file:/run/secrets/other")),
    needs = Vector(
      Need.Port(8080),
      Need.Volume("/app/data"),
      Need.Database(Engine.Postgres, "16", "shop"),
      Need.Cache(Engine.Redis, "7"),
      Need.Dns("shop.example.com"),
      Need.Tls(TlsMode.Acme)),
    scale = Scale(3))

  private val worker = Service(
    name = "worker",
    run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
    needs = Vector(Need.Neighbour("web")))

  private val shop = Deployment("shop", Vector(web, worker))

  private def files(d: Deployment = shop): Map[String, String] =
    Cluster.render(d).getOrElse(fail(s"refused: ${Cluster.render(d)}")).toMap

  test("a service becomes a Deployment, a Service and a ConfigMap, one file each") {
    val f = files()
    assert(f.contains("Chart.yaml"), f.keys.toString)
    assert(f.contains("templates/web-deployment.yaml"), f.keys.toString)
    assert(f.contains("templates/web-service.yaml"), f.keys.toString)
    assert(f.contains("templates/web-config.yaml"), f.keys.toString)
    // a service with no ports gets no Service, because there is
    // nothing to reach it on
    assert(!f.contains("templates/worker-service.yaml"), f.keys.toString)
    // per service, not one `range` over a map: the file an operator
    // opens is the file helm rendered
    assert(f.contains("templates/worker-deployment.yaml"), f.keys.toString)
  }

  test("the settings are a ConfigMap taken whole, and the database URL is built where the engine is known") {
    val cm = files()("templates/web-config.yaml")
    assert(cm.contains("""OKAY_PAGES: "/app/pages""""), cm)
    assert(cm.contains("""DB_URL: "jdbc:postgresql://{{ .Release.Name }}-web-db:5432/shop""""), cm)
    assert(cm.contains("""CACHE_URL: "{{ .Release.Name }}-web-cache:6379""""), cm)
    // the pod takes it whole rather than naming each key twice
    assert(files()("templates/web-deployment.yaml").contains("configMapRef"), "no envFrom")
  }

  test("THE SECRET IS NOT TEMPLATED: an upgrade cannot overwrite the operator's with blanks") {
    val f = files()
    for (path, content) <- f if path.startsWith("templates/") do
      assert(!content.contains("kind: Secret"), s"$path templates a Secret, and helm upgrade would blank it")
    // it is REFERENCED instead
    val dep = f("templates/web-deployment.yaml")
    assert(dep.contains("secretKeyRef"), dep)
    assert(dep.contains("name: {{ .Release.Name }}-secrets"), dep)
    assert(dep.contains("key: ADMIN_TOKEN"), dep)
    // and created by hand, once, with the command spelled out
    val sh = f("secrets.sh")
    assert(sh.contains("kubectl create secret generic"), sh)
    assert(sh.contains("ADMIN_TOKEN"), sh)
    assert(sh.contains("crash-looping"), sh)
  }

  test("a file: reference needs no plumbing here, and no rendered file carries a value") {
    val f = files()
    // file:/run/secrets/other resolves inside the process
    assert(!f.values.exists(_.contains("/run/secrets/other")), "a file: reference was wired as an env var")
    for (path, content) <- f do
      assert(!content.contains("hunter2"), path)
      assert(!content.contains("local-only"), s"$path carries the laptop target's fixed password")
  }

  test("the database is rendered, and the file says what it is") {
    val db = files()("templates/web-db.yaml")
    assert(db.contains("kind: StatefulSet"), db)
    assert(db.contains("replicas: 1"), db)
    assert(db.contains("ONE REPLICA, A PVC, AND NO BACKUPS"), db)
    assert(db.contains("point the service's"), db)
    // the password comes from the Secret nobody templated
    assert(db.contains("POSTGRES_PASSWORD"), db)
    assert(db.contains("secretKeyRef"), db)
    assert(!db.contains("value: \"local-only\""), db)
    // and the key is in the script that creates it
    assert(files()("secrets.sh").contains(Cluster.passwordKey(web, web.databases.head)), files()("secrets.sh"))
  }

  test("a cache has no volume: losing it costs a warm-up, not data") {
    val c = files()("templates/web-cache.yaml")
    assert(c.contains("kind: Deployment"), c)
    assert(!c.contains("PersistentVolumeClaim"), c)
    assert(!c.contains("volumeClaimTemplates"), c)
  }

  test("a self-signed certificate is REFUSED here, and the refusal names the two that work") {
    val d = shop.copy(services = Vector(web.copy(needs =
      web.needs.filterNot(_.isInstanceOf[Need.Tls]) :+ Need.Tls(TlsMode.SelfSigned))))
    Cluster.render(d) match
      case Right(_) => fail("a self-signed certificate was rendered behind an ingress")
      case Left(m) =>
        assert(m.contains("web"), m)
        assert(m.contains("browser warning"), m)
        assert(m.contains("Acme") && m.contains("Files"), m)
  }

  test("each TLS mode is a different ingress, and Proxy has no tls stanza at all") {
    def ingressWith(mode: TlsMode): String =
      val d = shop.copy(services = Vector(web.copy(needs =
        web.needs.filterNot(_.isInstanceOf[Need.Tls]) :+ Need.Tls(mode))))
      files(d)("templates/web-ingress.yaml")

    val acme = ingressWith(TlsMode.Acme)
    assert(acme.contains("cert-manager.io/cluster-issuer"), acme)
    assert(acme.contains("  tls:"), acme)

    val fromFiles = ingressWith(TlsMode.Files)
    assert(!fromFiles.contains("cert-manager"), fromFiles)
    assert(fromFiles.contains("secretName: {{ .Release.Name }}-web-tls"), fromFiles)

    // behind a proxy the ingress IS the proxy; terminating TLS is the
    // cluster's business, not this chart's
    val proxy = ingressWith(TlsMode.Proxy)
    assert(!proxy.contains("tls:"), proxy)
    assert(proxy.contains("host: \"shop.example.com\""), proxy)
  }

  test("the ClusterIssuer is a VALUE with the conventional default, not a constant in a template") {
    val v = files()("values.yaml")
    assert(v.contains("ingress:"), v)
    assert(v.contains("""issuer: "letsencrypt-prod""""), v)
    // and it is absent when nothing asks for it -- helm lint refused
    // the first rendering for exactly the opposite mistake
    val plain = shop.copy(services = Vector(web.copy(needs = Vector(Need.Port(8080)))))
    assert(!files(plain)("values.yaml").contains("ingress:"), files(plain)("values.yaml"))
  }

  test("what an operator overrides is in values; what they read is in the templates") {
    val v = files()("values.yaml")
    assert(v.contains("""repository: "ghcr.io/okay/web""""), v)
    assert(v.contains("replicaCount: 3"), v)
    assert(v.contains("""dataSize: "1Gi""""), v)
    val dep = files()("templates/web-deployment.yaml")
    assert(dep.contains("replicas: {{ $v.replicaCount }}"), dep)
    assert(dep.contains("""image: "{{ $v.image.repository }}:{{ $v.image.tag }}""""), dep)
  }

  test("a neighbour renders nothing: inside a cluster the service name IS the DNS name") {
    val f = files()
    assert(!f.keys.exists(_.contains("worker-config")), f.keys.toString)
    assert(!f.values.exists(_.contains("Neighbour")), "a neighbour leaked into a manifest")
  }

  test("the volume becomes a PVC and its mount, sized from values") {
    val f = files()
    val pvc = f("templates/web-data-pvc.yaml")
    assert(pvc.contains("kind: PersistentVolumeClaim"), pvc)
    assert(pvc.contains("storage: {{ $v.dataSize }}"), pvc)
    val dep = f("templates/web-deployment.yaml")
    assert(dep.contains("""mountPath: "/app/data""""), dep)
    assert(dep.contains("claimName: {{ .Release.Name }}-web-data"), dep)
  }

  test("the target names its tools and applies through helm, never reimplementing it") {
    assertEquals(Cluster.requires(shop), Vector("kubectl", "helm"))
    val dir = java.nio.file.Path.of("/srv/shop/deploy/cluster")
    assertEquals(Cluster.up(dir).take(4), Vector("helm", "upgrade", "--install", "shop"))
    assertEquals(Cluster.down(dir), Vector("helm", "uninstall", "shop"))
    assert(Targets.byName("cluster").isDefined)
  }

  test("a deployment with no secrets gets a script that says there is nothing to create") {
    val plain = Deployment("plain", Vector(Service("only", Run.Image("nginx", "alpine"))))
    val sh = files(plain)("secrets.sh")
    assert(sh.contains("reads no secrets"), sh)
    assert(!sh.contains("kubectl create secret"), sh)
  }
