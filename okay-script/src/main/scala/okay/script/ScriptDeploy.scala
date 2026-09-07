package okay.script

import okay.deploy.{Copy, Deploy, Deployment, Env, Health, Image, Need, Run, Service, Settings, Targets, TlsMode}

/**
 * okay-script's own deployment, as the value it is (specs/deploy.md):
 * a container that runs `okay.script.Serve` over a directory of
 * markdown pages -- "the page is the deployment", packaged
 * (okay-script-image).
 *
 * `okay-script/deploy/` is this value rendered; regenerate with
 * `sbt "okayScript/runMain okay.script.ScriptDeploy"`, and
 * `TestScriptDeploy` refuses a drift between the two.
 *
 * The image carries the worked example as its pages so that it RUNS
 * out of the box and its own smoke test is a store; a real deployment
 * mounts its own directory over `/app/pages` (compose: a volume;
 * Kubernetes: a ConfigMap or a PVC) and changes nothing else. The
 * pages are read at request time, so a mounted directory that changes
 * is a site that changes -- the hot-reload half of "a new JSP", now
 * true of the container too.
 */
object ScriptDeploy:
  val spec: Deploy = Deploy(
    name = "okay-script",
    module = "okayScript",
    moduleDir = "okay-script",
    mainClass = "okay.script.Serve",
    port = 8080,
    image = Image("okay/script", "local"),
    env = Vector(
      // the entrypoint takes no arguments, so the pages directory and
      // the port ride in as configuration (Serve reads them when it
      // is given no command line)
      Env("OKAY_PAGES", "/app/pages"),
      Env("OKAY_PORT", "8080"),
      // /healthz, /stats and /metrics beside the pages -- the health
      // and metrics wiring below is what asks for them
      Env("OKAY_OPS", "1"),
      // NOT OKAY_DATA: /app belongs to root and the process runs as
      // `okay`, so a store path baked in here would be a container
      // that crashes on its first boot. A deployment that wants
      // sessions and the application scope across a restart mounts a
      // WRITABLE volume and sets OKAY_DATA to it -- one line in
      // compose or in the Helm values, and nothing else changes.
    ),
    // okay-script has no /readyz of its own: a Site is ready when it
    // is live (the pages were compiled before the port was bound)
    health = Health(livenessPath = "/healthz", readinessPath = "/healthz"),
    extraCopy = Vector(Copy("okay-script/examples/site", "/app/pages")))

  /**
   * The same deployment in the model specs/deployment.md defines --
   * a system of services and their needs, rather than one process
   * and its env pairs (deploy-model).
   *
   * Both values live here on purpose while the model is being
   * proven: `spec` still renders the Dockerfile and the single-service
   * Helm chart that exist, `system` renders every target in
   * `Targets.all`, and having them side by side in one real
   * application is what shows the new model can say what the old one
   * said. The old chart's retirement is its own task, because other
   * modules render through `Deploy` too. The env pairs become `Settings`, the `/app/pages`
   * copy becomes what it always was -- a volume the deployment
   * mounts -- and the port stops being written twice.
   */
  val system: Deployment = Deployment(
    name = "okay-script",
    services = Vector(Service(
      name = "web",
      run = Run.Module("okayScript", "okay-script", "okay.script.Serve"),
      // the settings are DERIVED from the value the program itself
      // reads (script-config): a field renamed in Serve.Config is
      // renamed here, and a name this deployment could invent does
      // not exist. Only what differs from the program's own defaults
      // is written -- a unit file restating a default is a lie
      // waiting for the default to change.
      settings = Settings.of(Serve.Config(
        pages = "/app/pages",
        port = 8080,
        // /healthz, /stats and /metrics beside the pages
        ops = true), Serve.Config.prefix).only("OKAY_PAGES", "OKAY_PORT", "OKAY_OPS"),
      needs = Vector(
        Need.Port(8080),
        // NOT baked into the image (script-tls): /app belongs to root
        // and the process runs as `okay`, so a store path in the image
        // is a container that crashes on its first boot. As a VOLUME
        // it is the deployment's, which is what it always was.
        Need.Volume("/app/data", name = "data"),
        Need.Tls(TlsMode.Proxy)),
      // a Site is ready when it is live: the pages were compiled
      // before the port was bound (okay-script-warm)
      health = Health(livenessPath = "/healthz", readinessPath = "/healthz"))))

  def main(args: Array[String]): Unit =
    val root = Deploy.repoRoot()
    Deploy.write(spec, root).foreach(p => println(s"wrote $p"))
    for target <- Targets.all do
      Deployment.write(system, target, root) match
        case Right(paths) => paths.foreach(p => println(s"wrote $p"))
        case Left(msg) => System.err.println(s"okay-script: the ${target.name} target refused: $msg")
