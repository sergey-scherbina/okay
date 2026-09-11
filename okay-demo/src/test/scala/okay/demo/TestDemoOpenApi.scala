package okay.demo

import okay.*
import okay.given
import okay.codec.Json
import okay.http.Request

/**
 * The committed `okay-demo/openapi.json` IS the rendered document —
 * the drift test, the shape TestDemoDeploy has for the deployment
 * (specs/openapi.md stage 2).
 *
 * The reader this protects is not in this repository: an integrator
 * writing a client. The proxy for them is a diff in review, and a
 * diff only appears if the file is committed and checked.
 */
class TestDemoOpenApi extends munit.FunSuite:

  private def withCaps[A](f: (okay.conf.Secrets, Board, okay.persist.Store) ?=> A): A =
    val store = Board.store(":memory:")
    given okay.persist.Store = store
    given Board = Board(Board.topicOf(store))
    given okay.conf.Secrets = okay.conf.Secrets.memory(Map.empty)
    f

  test("okay-demo/openapi.json does not drift from the router that answers") {
    val rendered = withCaps(Json.print(DemoOpenApi.document))
    val committed = java.nio.file.Files.readString(
      okay.deploy.Deployment.repoRoot().resolve("okay-demo/openapi.json")).trim
    assertEquals(committed, rendered,
      "regenerate with: sbt \"okayDemo/runMain okay.demo.DemoOpenApi\"")
  }

  test("the service serves the document and the page") {
    withCaps {
      val routes = ChatDemo.routes(okay.chat.Chat.scripted, 512)
      val doc = Async.run(routes(Request.get("/openapi.json"))).runWith
      assertEquals(doc.status, 200)
      val page = Async.run(routes(Request.get("/openapi"))).runWith
      assertEquals(page.status, 200)
      val html = Async.run(okay.http.Http.text(page)).runWith
      assert(html.contains("/board.json"), html.take(200))
      assert(!html.contains("<script"), "the page must not need javascript")
    }
  }

  test("every path the document names is a path the router dispatches") {
    withCaps {
      val routes = DemoOpenApi.router.routes
      val paths = DemoOpenApi.document match
        case Json.JObj(fs) => fs.collectFirst { case ("paths", Json.JObj(ps)) => ps.map(_._1) }.getOrElse(Vector.empty)
        case _ => Vector.empty
      assert(paths.nonEmpty)
      // a template's variable is filled with something concrete
      paths.map(_.replace("{email}", "a@b.c")).foreach(p =>
        assert(routes.isDefinedAt(Request.get(p)), s"the document names $p and the router does not dispatch it"))
    }
  }
