package okay.script

import okay.deploy.Deployment

/** the committed okay-script/deploy IS the rendered
 * ScriptDeploy.system -- a hand edit or a stale regeneration fails
 * here, by file name */
class TestScriptDeploy extends munit.FunSuite:

  test("okay-script/deploy does not drift from ScriptDeploy.system, on every target that renders") {
    for target <- okay.deploy.Targets.all do
      okay.deploy.Deployment.drift(ScriptDeploy.system, target, Deployment.repoRoot()) match
        case Right(files) => assertEquals(files, Vector.empty, s"the ${target.name} target has drifted")
        // a target that REFUSES this deployment has nothing committed
        // and nothing to drift; what it must have is a reason
        case Left(why) =>
          assert(why.nonEmpty && why.length > 20, s"the ${target.name} target refused without saying why: $why")
  }

  test("fly refuses okay-script by name, because nobody has chosen a region") {
    okay.deploy.Paas.Fly.render(ScriptDeploy.system) match
      case Right(_) => fail("a region was invented for okay-script")
      case Left(why) =>
        assert(why.contains("region"), why)
        assert(why.contains("Need.Region"), why)
    // and the two that do not need one are committed
    assert(java.nio.file.Files.isRegularFile(
      Deployment.repoRoot().resolve("okay-script/deploy/render/render.yaml")))
    assert(java.nio.file.Files.isRegularFile(
      Deployment.repoRoot().resolve("okay-script/deploy/railway/web/railway.json")))
  }

  test("the port and the pages are each written once") {
    val d = ScriptDeploy.system
    val web = d.service("web").getOrElse(fail("no web service"))
    assertEquals(web.mainPort, Some(8080))
    assertEquals(web.settings.env.toMap.get("OKAY_PAGES"), Some("/app/pages"))
    assertEquals(web.settings.env.toMap.get("OKAY_OPS"), Some("1"))
    // the data directory is a VOLUME rather than an image path -- the
    // reason OKAY_DATA was never baked into the image (script-tls)
    assertEquals(web.volumes.map(_.path), Vector("/app/data"))
    // and the value round-trips, since the CLI will read it as JSON
    assertEquals(okay.deploy.Deployment.read(okay.deploy.Deployment.json(d)), Right(d))
  }

  test("the image runs Serve over the pages it carries, with the ops routes on") {
    val web = ScriptDeploy.system.service("web").getOrElse(fail("no web service"))
    assertEquals(web.run.asInstanceOf[okay.deploy.Run.Module].mainClass, "okay.script.Serve")
    assertEquals(web.settings.env.toMap.get("OKAY_OPS"), Some("1"))
    val (rel, dockerfile) = Deployment.image(ScriptDeploy.system) match
      case Vector(one) => one
      case other => fail(s"expected one image, got ${other.map(_._1)}")
    assertEquals(rel, "okay-script/deploy/Dockerfile")
    assert(dockerfile.contains("COPY --from=build /src/okay-script/examples/site /app/pages"), dockerfile)
    assert(dockerfile.contains("okayScript/assembly"), dockerfile)
    // and the committed one IS this one
    assertEquals(java.nio.file.Files.readString(Deployment.repoRoot().resolve(rel)), dockerfile)
  }

  test("Serve reads OKAY_PAGES and OKAY_PORT when it is given no command line") {
    val root = java.nio.file.Files.createTempDirectory("okay-script-image-")
    try
      val env = Map("OKAY_PAGES" -> root.toString, "OKAY_PORT" -> "8080")
      assertEquals(Serve.parse(Array.empty, env.get).map(a => (a.root.toString, a.port)), Right((root.toString, 8080)))
      // an explicit command line still wins over the environment
      assertEquals(Serve.parse(Array(root.toString, "9999"), env.get).map(_.port), Right(9999))
      // and neither is still a usage refusal, not a guess
      assert(Serve.parse(Array.empty, _ => None).left.exists(_.startsWith("usage")))
    finally java.nio.file.Files.deleteIfExists(root): Unit
  }
