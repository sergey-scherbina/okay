package okay.demo

import okay.deploy.{Deployment, Targets}

/** the committed okay-demo/deploy IS the rendered DemoDeploy.system —
 * a hand edit or a stale regeneration fails here, by file name */
class TestDemoDeploy extends munit.FunSuite:

  test("okay-demo/deploy does not drift from DemoDeploy.system, on every target that renders") {
    for target <- Targets.all do
      Deployment.drift(DemoDeploy.system, target, Deployment.repoRoot()) match
        case Right(files) => assertEquals(files, Vector.empty, s"the ${target.name} target has drifted")
        case Left(why) => assert(why.length > 20, s"the ${target.name} target refused without saying why: $why")
  }

  test("the image is rendered once, beside the targets rather than inside one") {
    val (rel, content) = Deployment.image(DemoDeploy.system) match
      case Vector(one) => one
      case other => fail(s"expected one image, got ${other.map(_._1)}")
    assertEquals(rel, "okay-demo/deploy/Dockerfile")
    val committed = java.nio.file.Files.readString(Deployment.repoRoot().resolve(rel))
    assertEquals(content, committed)
    // the Scala.js bundle is linked in the build stage and copied in
    // beside the jar: the two fields the new model gained for this
    assert(content.contains("okayChatWebJS/fastLinkJS"), content)
    assert(content.contains("main.js /app/app.js"), content)
  }

  test("the settings carry the names this program actually reads") {
    // `Settings.of("okayChat")("port")` would be OKAYCHAT_PORT, which
    // nothing reads: the prefix is uppercased, not split
    val env = DemoDeploy.system.service("chat").getOrElse(fail("no chat service")).settings.env.toMap
    assertEquals(env.get("OKAY_CHAT_PORT"), Some("8090"))
    assertEquals(env.get("OKAY_CHAT_APP"), Some("/app/app.js"))
    assert(!env.keys.exists(_.startsWith("OKAYCHAT")), env.keys.toString)
  
  /**
   * The deployment reads the application's ROOT (specs/di.md stage 3,
   * needs-runtime). This app asks the place for nothing: it opens its
   * own store, builds its own transport and reads its own secrets,
   * and its one remaining input is a `Timer`, which the process
   * brings and no target can. The day the root gains a database it
   * did not provision, this stops compiling until someone declares
   * what that is — which is the point.
   */
  test("the demo's root asks the place for nothing, and the Timer is not a need") {
    assertEquals(okay.deploy.Needs.of[ChatDemo.Root], Vector.empty)
  }
}
