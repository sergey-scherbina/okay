package okay.demo

import okay.deploy.{Copy, Deploy, Env, Image}

/**
 * DemoChat's deployment, as the value it is (specs/deploy.md): the
 * ONLY place that knows this application's port variable, its log
 * directory, its image name. `okay-demo/deploy/` is this value
 * rendered — regenerate with `sbt "okayDemo/runMain okay.demo.DemoDeploy"`;
 * TestDemoDeploy refuses a drift between the two.
 */
object DemoDeploy:
  val spec: Deploy = Deploy(
    name = "demo-chat",
    module = "okayDemo",
    moduleDir = "okay-demo",
    mainClass = "okay.demo.ChatDemo",
    port = 8090,
    image = Image("okay/demo-chat", "local"),
    env = Vector(
      Env("OKAY_CHAT_PORT", "8090"),
      Env("OKAY_CHAT_LOG", ":memory:"),   // a real deployment mounts a volume here
      // the React bundle, linked and copied in below — Chat.appJs
      // already reads this env var first (demo-package)
      Env("OKAY_CHAT_APP", "/app/app.js"),
    ),
    // one-command run (demo-package): the build stage links the
    // React frontend too, and its output rides into the image next
    // to the jar — no separate node/dev-server step
    extraBuild = Vector("okayChatWebJS/fastLinkJS"),
    extraCopy = Vector(Copy(
      "okay-demo/web/.js/target/scala-*/*-fastopt/main.js", "/app/app.js")))

  def main(args: Array[String]): Unit =
    // `run` forks with the MODULE directory as cwd; the deploy dir is
    // named relative to the repository root, so find that first
    val written = Deploy.write(spec, Deploy.repoRoot())
    written.foreach(p => println(s"wrote $p"))
