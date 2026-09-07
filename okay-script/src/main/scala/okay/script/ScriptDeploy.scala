package okay.script

import okay.deploy.{Copy, Deploy, Env, Health, Image}

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

  def main(args: Array[String]): Unit =
    val written = Deploy.write(spec, Deploy.repoRoot())
    written.foreach(p => println(s"wrote $p"))
