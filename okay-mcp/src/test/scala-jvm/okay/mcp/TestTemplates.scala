package okay.mcp

import okay.*
import okay.given
import okay.codec.Json

/**
 * Resource templates: one declaration standing for unbounded uris —
 * `expand` and its reverse `matches` are what make that sentence
 * true, and the end-to-end serves a whole file tree off one line.
 */
class TestTemplates extends munit.FunSuite {

  import Mcp.Template

  test("expand and matches are inverses on level 1, encoding included") {
    val t = "okay://file/{path}/line/{n}"
    val vars = Map("path" -> "a b.scala", "n" -> "42")
    val uri = Template.expand(t, vars)
    assertEquals(uri, "okay://file/a%20b.scala/line/42")
    assertEquals(Template.matches(t, uri), Some(vars))
  }

  test("matches never guesses") {
    val t = "okay://file/{path}"
    assertEquals(Template.matches(t, "okay://other/x"), None)
    assertEquals(Template.matches(t, "okay://file/a/b"), None)   // '/' does not cross
    assertEquals(Template.matches(t, "okay://file/"), Some(Map("path" -> "")))
    assertEquals(Template.matches("okay://exact", "okay://exact"), Some(Map.empty))
    assertEquals(Template.matches("okay://exact", "okay://exact/more"), None)
  }

  test("templates round-trip the wire, and alone they declare resources") {
    val t = Template("okay://file/{path}", "a file", "by path", Some("text/plain"))
    assertEquals(McpDocs.templateOf(Json.parse(Json.print(McpDocs.templateJson(t)))), Some(t))

    val serving = Server.Serving(Mcp.Info("t", "1"), templates = Seq(t))
    val out = !.run(Writer.run(through(Writer.of(List(
      Rpc.Request(Json.JNum(1), Mcp.Initialize, Json.JObj(Vector.empty)),
      Rpc.Request(Json.JNum(2), Mcp.ResourcesTemplates, Json.JObj(Vector.empty)))))(
      Server.serve(serving))))._1
    val Rpc.Answer(_, init) = out(0): @unchecked
    assert(Mcp.capability(init, "resources"), "templates alone did not declare resources")
    val Rpc.Answer(_, listed) = out(1): @unchecked
    assertEquals(McpDocs.templatesOf(listed), Seq(t))
  }

  test("a server serves a template: one declaration, unbounded uris — with completion") {
    val files = Map("Main.scala" -> "object Main", "Util.scala" -> "object Util")
    val tpl = "okay://file/{path}"
    val serving = Server.Serving(Mcp.Info("files", "1"),
      templates = Seq(Mcp.Template(tpl, "a file")),
      read = uri => Mcp.Template.matches(tpl, uri)
        .flatMap(vars => files.get(vars.getOrElse("path", ""))),
      complete = Some { c =>
        c.ref match
          case Mcp.Ref.Resource(u) if u == tpl && c.argument == "path" =>
            files.keys.toVector.sorted.filter(_.startsWith(c.value))
          case _ => Vector.empty
      })

    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = async(out.send(line))
      def lines: Source[String] = Writer.of(in)
    Async.spawn(Server.run(link(down, up), serving)): Unit
    val s = Client.connect(link(up, down), Mcp.Info("c", "1")).runWith

    val t = s.templates.runWith.head
    // complete the variable, expand, read — the whole usage loop
    val names = s.complete(Mcp.Ref.Resource(t.uriTemplate), "path", "M").runWith
    assertEquals(names, Vector("Main.scala"))
    val uri = Mcp.Template.expand(t.uriTemplate, Map("path" -> names.head))
    assertEquals(s.read(uri).runWith, Some("object Main"))
    // a uri outside the template refuses like any unknown resource
    assertEquals(s.read("okay://file/Nope.scala").runWith, None)
  }
}
