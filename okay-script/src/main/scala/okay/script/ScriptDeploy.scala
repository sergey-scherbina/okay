package okay.script

import okay.deploy.{Copy, Deployment, Health, Need, Run, Service, Settings, Targets, TlsMode}

/**
 * okay-script's own deployment, as the value it is
 * (specs/deployment.md):
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

  /**
   * okay-script's whole deployment as ONE value
   * (specs/deployment.md): the services and what each of them needs.
   *
   * There used to be two values here — a `Deploy` for the Dockerfile,
   * the compose file and a single-service Helm chart, and this one
   * for the new targets — side by side while the new model was being
   * proven. It is proven (deploy-old-helm-retired), and one
   * application committing two Helm charts was exactly the drift this
   * repository has a rule against.
   */
  val system: Deployment = Deployment(
    name = "okay-script",
    services = Vector(Service(
      name = "web",
      // the entrypoint takes no arguments, so the pages directory and
      // the port ride in as settings (Serve reads them when it is
      // given no command line); the examples site rides into the
      // image beside the jar
      run = Run.Module("okayScript", "okay-script", "okay.script.Serve",
        extraCopy = Vector(Copy("okay-script/examples/site", "/app/pages"))),
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
    val root = Deployment.repoRoot()
    for result <- Deployment.writeAll(system, Targets.all, root) do
      result match
        case Right(paths) => paths.foreach(p => println(s"wrote $p"))
        case Left(msg) => System.err.println(s"okay-script: a target refused: $msg")
