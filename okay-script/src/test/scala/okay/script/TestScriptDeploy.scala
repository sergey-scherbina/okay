package okay.script

import okay.deploy.Deploy

/** the committed okay-script/deploy IS the rendered ScriptDeploy.spec
 * -- a hand edit or a stale regeneration fails here, by file name */
class TestScriptDeploy extends munit.FunSuite:
  test("okay-script/deploy does not drift from ScriptDeploy.spec") {
    assertEquals(Deploy.drift(ScriptDeploy.spec, Deploy.repoRoot()), Vector.empty)
  }

  test("okay-script/deploy does not drift from ScriptDeploy.system, on either new target") {
    for target <- Vector(okay.deploy.Targets.Laptop, okay.deploy.Targets.Host) do
      assertEquals(okay.deploy.Deployment.drift(ScriptDeploy.system, target, okay.deploy.Deploy.repoRoot()),
        Right(Vector.empty), s"the ${target.name} target has drifted")
  }

  test("the new model says what the old value said: the port and the pages, each written once") {
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
    val d = ScriptDeploy.spec
    assertEquals(d.mainClass, "okay.script.Serve")
    assertEquals(d.env.find(_.name == "OKAY_PAGES").map(_.value), Some("/app/pages"))
    assertEquals(d.env.find(_.name == "OKAY_OPS").map(_.value), Some("1"))
    val dockerfile = okay.deploy.Dockerfile.render(d)
    assert(dockerfile.contains("COPY --from=build /src/okay-script/examples/site /app/pages"), dockerfile)
    assert(dockerfile.contains("okayScript/assembly"), dockerfile)
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
