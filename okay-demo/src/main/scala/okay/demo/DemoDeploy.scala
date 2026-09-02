package okay.demo

import okay.deploy.{Deploy, Env, Image}

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
    ))

  def main(args: Array[String]): Unit =
    // `run` forks with the MODULE directory as cwd; the deploy dir is
    // named relative to the repository root, so find that first
    val written = Deploy.write(spec, Deploy.repoRoot())
    written.foreach(p => println(s"wrote $p"))
