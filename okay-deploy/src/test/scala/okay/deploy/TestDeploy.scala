package okay.deploy

import java.nio.file.Files

/** the renderers are PURE and pinned; write/drift round-trip */
class TestDeploy extends munit.FunSuite:

  val d = Deploy("svc", "okaySvc", "okay-svc", "okay.svc.Main", 8080,
    Image("okay/svc", "v1"), env = Vector(Env("OKAY_PORT", "8080"), Env("OKAY_LOG", ":memory:")),
    resources = Some(Resources("100m", "256Mi", "1", "512Mi")))

  test("the Dockerfile builds ONE module's jar and runs it as a non-root user") {
    val f = Dockerfile.render(d)
    assert(f.contains("""RUN sbt "okaySvc/assembly""""), f)
    assert(f.contains("COPY --from=build /src/okay-svc/target/scala-*/app.jar /app/app.jar"), f)
    assert(f.contains("USER okay") && f.contains("EXPOSE 8080"), f)
    assert(!f.contains("ARG "), "nothing is left to a build arg: the value decided everything")
  }

  test("values.yaml carries every knob as a quoted scalar; the chart carries none") {
    val v = Helm.values(d)
    assert(v.contains("""repository: "okay/svc""""), v)
    assert(v.contains("""  - name: OKAY_LOG
    value: ":memory:"""".stripMargin), v)
    assert(v.contains("""livenessPath: "/healthz""""), v)
    assert(v.contains("port: 8080"), v)
    assert(v.contains("""cpu: "100m""""), v)
    val chart = Deploy.files(d).collect { case (p, c) if p.startsWith("helm/templates/") => c }.mkString
    assert(!chart.contains("okay-svc") && !chart.contains("OKAY_"), "the chart templates know no application")
  }

  test("compose builds from the module's own Dockerfile with the repo as context") {
    val c = Compose.render(d)
    assert(c.contains("context: ../..") && c.contains("dockerfile: okay-svc/deploy/Dockerfile"), c)
    assert(c.contains(""""8080:8080""""), c)
  }

  test("write then drift is empty; a hand edit is named; a missing file is named") {
    val root = Files.createTempDirectory("okay-deploy")
    Deploy.write(d, root)
    assertEquals(Deploy.drift(d, root), Vector.empty)
    val f = root.resolve(d.dir).resolve("helm/values.yaml")
    Files.writeString(f, Files.readString(f) + "\n# hand edit\n")
    Files.delete(root.resolve(d.dir).resolve("compose.yaml"))
    assertEquals(Deploy.drift(d, root).sorted, Vector("compose.yaml", "helm/values.yaml"))
  }

  test("a Deploy is a value with a Schema: JSON-inspectable") {
    val json = okay.codec.Json.encode(summon[okay.codec.Schema[Deploy]])(d)
    assert(json.contains("\"okaySvc\"") && json.contains("\"/healthz\""), json)
  }
