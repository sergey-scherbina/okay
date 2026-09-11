package okay.demo


import okay.{Module, given}
import okay.deploy.{Copy, Deployment, Need, Needs, Run, Service, Settings, Targets}

/**
 * DemoChat's deployment as ONE value (specs/deployment.md): the ONLY
 * place that knows this application's port, its log directory, its
 * image name.
 *
 * `okay-demo/deploy/` is this value rendered — regenerate with
 * `sbt "okayDemo/runMain okay.demo.DemoDeploy"`; TestDemoDeploy
 * refuses a drift between the two.
 *
 * Ported from specs/deploy.md's `Deploy` when that model was retired
 * (deploy-old-helm-retired). Porting it is what found the two things
 * the new model was missing — `extraBuild`/`extraCopy`, because this
 * application links a Scala.js bundle in the build stage, and
 * `metricsPath`, which the old chart annotated a pod with.
 */
object DemoDeploy:
  /** the settings, ONCE: the process reads them as environment, and
   * the needs above are derived from the same values */
  val settings: Settings = Settings.of("okay")(
    "chatPort" -> "8090",
    // the board's log, on a volume the store module declares for it.
    // Before module-facts this key was `chatLog` — OKAY_CHAT_LOG, the
    // two-node log DIR — set to ":memory:", so the container wrote its
    // board to an unmounted file while the manifest believed it ran
    // in memory
    "chatDb" -> "/app/data/okay-board.log",
    // Chat.appJs reads this first (demo-package)
    "chatApp" -> "/app/app.js")
  private val conf: ChatDemo.ChatConf = ChatDemo.ChatConf.from(settings.env.toMap.get)

  val system: Deployment = Deployment(
    name = "demo-chat",
    services = Vector(Service(
      name = "chat",
      // one-command run (demo-package): the build stage links the
      // React frontend too, and its output rides into the image next
      // to the jar — no separate node/dev-server step
      run = Run.Module("okayDemo", "okay-demo", "okay.demo.ChatDemo",
        extraBuild = Vector("okayChatWebJS/fastLinkJS"),
        extraCopy = Vector(Copy(
          "okay-demo/web/.js/target/scala-*/*-fastopt/main.js", "/app/app.js"))),
      // the prefix is `okay` and the fields carry `chat`, because
      // `Conf.envName` uppercases the prefix WITHOUT splitting it:
      // Settings.of("okayChat")("port") is OKAYCHAT_PORT, which no
      // part of this program reads. Caught by writing the port down
      // twice and looking.
      settings = settings,
      // what the APPLICATION declares it needs from the place, read off
      // its own modules with the settings above as their config and
      // nothing opened (module-facts) — the board's volume comes from
      // the component that opens the board — plus what only the place
      // can say
      needs = Needs.declared(Module.value[ChatDemo.ChatConf](conf) and ChatDemo.wiring) :+ Need.Port(8090))))


  def main(args: Array[String]): Unit =
    val root = Deployment.repoRoot()
    for result <- Deployment.writeAll(system, Targets.all, root) do
      result match
        case Right(paths) => paths.foreach(p => println(s"wrote $p"))
        case Left(msg) => System.err.println(s"okay-demo: a target refused: $msg")
