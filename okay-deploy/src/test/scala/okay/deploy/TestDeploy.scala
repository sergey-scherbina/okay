package okay.deploy

/**
 * What survived specs/deploy.md: the Dockerfile every target's image
 * comes out of.
 *
 * The compose and Helm tests that stood beside these went with the
 * renderers they covered — `laptop` and `cluster` say the same thing
 * for more than one service, and TestDeployment/TestCluster test
 * those.
 */
class TestDeploy extends munit.FunSuite:

  private val svc = Service(
    name = "svc",
    run = Run.Module("okaySvc", "okay-svc", "okay.svc.Main"),
    settings = Settings.of("okay")("port" -> "8080"),
    needs = Vector(Need.Port(8080)),
    resources = Some(Resources("100m", "256Mi", "1", "512Mi")))

  private val d = Deployment("svc", Vector(svc))

  private def dockerfile(s: Service = svc): String =
    Dockerfile.render(d, s, s.run.asInstanceOf[Run.Module])

  test("the Dockerfile builds ONE module's jar and runs it as a non-root user") {
    val f = dockerfile()
    assert(f.contains("""RUN sbt "okaySvc/assembly""""), f)
    assert(f.contains("COPY --from=build /src/okay-svc/target/scala-*/app.jar /app/app.jar"), f)
    assert(f.contains("USER okay") && f.contains("EXPOSE 8080"), f)
    assert(!f.contains("ARG "), "nothing is left to a build arg: the value decided everything")
  }

  test("extraBuild/extraCopy add a build task and a COPY line; empty is byte-identical (demo-package)") {
    val withExtras = svc.copy(run = Run.Module("okaySvc", "okay-svc", "okay.svc.Main",
      extraBuild = Vector("okayChatWebJS/fastLinkJS"),
      extraCopy = Vector(Copy("okay-demo/web/.js/target/scala-*/*-fastopt/main.js", "/app/app.js"))))
    val f = dockerfile(withExtras)
    assert(f.contains("""RUN sbt "okaySvc/assembly" "okayChatWebJS/fastLinkJS""""), f)
    assert(f.contains(
      "COPY --from=build /src/okay-demo/web/.js/target/scala-*/*-fastopt/main.js /app/app.js"), f)
    // the extra COPY lands BEFORE the user switches to non-root
    val jarAt = f.indexOf("app.jar /app/app.jar")
    val extraAt = f.indexOf("app.js")
    val userAt = f.indexOf("USER okay")
    assert(jarAt < extraAt && extraAt < userAt, f)
    // and with no extras the render is UNCHANGED
    assert(!dockerfile().contains("okayChatWebJS"))
  }

  test("a service with no port EXPOSEs none, rather than a zero") {
    val worker = svc.copy(needs = Vector.empty)
    val f = dockerfile(worker)
    assert(!f.contains("EXPOSE"), f)
    assert(f.contains("ENTRYPOINT"), f)
  }

  test("the image is a DEPLOYMENT-level rendering: one per module-run service, none for an image") {
    assertEquals(Deployment.image(d).map(_._1), Vector("okay-svc/deploy/Dockerfile"))
    val pulled = Deployment("pulled", Vector(Service("only", Run.Image("nginx", "alpine"))))
    assertEquals(Deployment.image(pulled), Vector.empty)
    val two = Deployment("two", Vector(svc, svc.copy(name = "other",
      run = Run.Module("okayOther", "okay-other", "okay.other.Main"))))
    assertEquals(Deployment.image(two).map(_._1),
      Vector("okay-svc/deploy/Dockerfile", "okay-other/deploy/Dockerfile"))
  }

  test("repoRoot is the nearest ancestor with a build.sbt") {
    val root = Deploy.repoRoot()
    assert(java.nio.file.Files.exists(root.resolve("build.sbt")), root.toString)
  }
