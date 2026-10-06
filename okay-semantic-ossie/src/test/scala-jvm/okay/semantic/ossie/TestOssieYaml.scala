package okay.semantic.ossie

import okay.codec.Json
import Json.*

class TestOssieYaml extends okay.testkit.Munit.Diagnosed:
  import Samples.*
  test("optional block YAML and portable JSON implementations read each other's output") {
    val document = Document.fromJson(raw).toOption.get
    val block = document.yaml(using SnakeYaml)
    note(block)
    val imported = Document.readYaml(block)(using SnakeYaml).toOption.get
    assertEquals(imported.raw,raw)
    assertEquals(Document.readJson(imported.json).toOption.get.raw,raw)
    assertEquals(Document.readYaml(document.yaml)(using SnakeYaml).toOption.get.raw,raw)
    assert(Document.readYaml(block).isLeft)
    assert(SnakeYaml.byName("snakeyaml").isRight)
    assert(Syntax.byName("alien").isLeft)
    assert(SnakeYaml.missing(new ClassLoader(null) {}).nonEmpty)
  }
  test("adapter refuses by name in a real JVM without its optional jar") {
    val locations = Vector(classOf[Syntax],classOf[Json],classOf[Option[?]],classOf[scala.deriving.Mirror],classOf[MissingYamlProbe])
      .map(c => java.nio.file.Path.of(c.getProtectionDomain.getCodeSource.getLocation.toURI).toString).distinct
    val executable = java.nio.file.Path.of(System.getProperty("java.home"),"bin","java").toString
    val child = new ProcessBuilder(executable,"-cp",locations.mkString(java.io.File.pathSeparator),classOf[MissingYamlProbe].getName)
      .redirectErrorStream(true).start()
    val finished = child.waitFor(30L,java.util.concurrent.TimeUnit.SECONDS)
    if !finished then child.destroyForcibly(): Unit
    assert(finished,"missing-jar probe exceeded 30 seconds")
    val output = new String(child.getInputStream.readAllBytes(),java.nio.charset.StandardCharsets.UTF_8)
    note(output)
    assertEquals(child.exitValue(),0,output)
    assert(output.contains("requires optional org.snakeyaml"),output)
  }
  test("real upstream Flights document validates and survives interchange") {
    val source = scala.io.Source.fromResource("flights.semantic_model.yaml")
    val text = try source.mkString finally source.close()
    val imported = Document.readYaml(text)(using SnakeYaml)
    note(imported.fold(_.mkString("\n"),_.name))
    val document = imported.toOption.get
    assert(document.datasets.nonEmpty)
    assertEquals(Document.readJson(document.json).toOption.get.raw,document.raw)
    val schema = scala.io.Source.fromResource("ossie-schema.json")
    val pinned = try schema.mkString finally schema.close()
    assertEquals(Json.parse(pinned),Json.parse(Pinned.schemaText))
  }
  test("YAML quotes, block strings, flow keys and unknown AI members remain data") {
    val text = """version: 0.2.0.dev0
name: sales
datasets:
  - name: orders
    source: orders
    primary_key: [id, region]
    fields:
      - name: id
        expression:
          dialects:
            - dialect: ANSI_SQL
              expression: 'id'
      - name: region
        expression:
          dialects:
            - dialect: ANSI_SQL
              expression: region
ai_context:
  instructions: |
    First line
    Second line
  vendor_note: {enabled: true, label: "[literal]"}
"""
    val document = Document.readYaml(text)(using SnakeYaml).toOption.get
    val instructions = Read.get(document.aiContext.get,"instructions").get
    assertEquals(instructions,JStr("First line\nSecond line\n"))
    assertEquals(document.datasets.head.primaryKey,Vector("id","region"))
    assertEquals(Document.readYaml(document.yaml(using SnakeYaml))(using SnakeYaml).toOption.get.raw,document.raw)
  }
  test("duplicate keys, aliases, custom tags, imprecise numbers and multiple documents fail") {
    val block = Document.fromJson(raw).toOption.get.yaml(using SnakeYaml)
    val cases = Vector(block + "\nname: duplicated\n",block + "\n---\nname: second\n",
      block.replace("1.25","9007199254740993"),"ai_context: &x {self: *x}\n",
      block.replace("\"sales\"","!custom sales"),block + "ai_context: {future: !!bool nonsense}\n","[[" * 200 + "0" + "]]" * 200)
    cases.foreach { text => val result = Document.readYaml(text)(using SnakeYaml); note(result.left.toOption.toString); assert(result.isLeft) }
  }
