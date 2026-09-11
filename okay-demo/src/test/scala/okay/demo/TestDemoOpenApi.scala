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

  test("no operation in the published document says `undeclared`") {
    // the openapi-serve lane shipped a document that said `undeclared`
    // six times out of six, because every handler built its own
    // Response. It is a regression guard now: a handler added with
    // `on`/`at` where a declaring combinator exists puts the word back.
    withCaps {
      val printed = Json.print(DemoOpenApi.document)
      assert(!printed.contains("undeclared"),
        "an operation declares no answer — use html/bytes/events/out, not on/at")
    }
  }

  test("every operation in the published document says what it is for") {
    // the derived id names the URL a second time; a reader deciding
    // whether an operation is the one they want needs a sentence, and
    // nothing but an author can write it (openapi-prose)
    withCaps {
      def obj(j: Json): Vector[(String, Json)] = j match
        case Json.JObj(fs) => fs
        case _ => Vector.empty
      def get(j: Json, k: String): Json = obj(j).collectFirst { case (`k`, v) => v }.getOrElse(Json.JNull)
      val ops = obj(get(DemoOpenApi.document, "paths")).flatMap((path, item) =>
        obj(item).map((method, op) => (s"$method $path", get(op, "summary"))))
      assert(ops.nonEmpty)
      ops.foreach { (name, summary) =>
        summary match
          case Json.JStr(t) => assert(t.length > 10, s"$name: a summary of '$t' says nothing")
          case _ => fail(s"$name has no summary — add .summarised(\"...\") to its declaration")
      }
    }
  }

  test("what the document says an operation answers is what the service sends") {
    // the law of openapi-media on a REAL service: for each operation,
    // the content-type on the wire is the media type the document
    // files it under. A declaration is worth what it costs to check.
    withCaps {
      val routes = DemoOpenApi.router.routes
      val doc = DemoOpenApi.document
      def obj(j: Json): Vector[(String, Json)] = j match
        case Json.JObj(fs) => fs
        case _ => Vector.empty
      def get(j: Json, k: String): Json = obj(j).collectFirst { case (`k`, v) => v }.getOrElse(Json.JNull)
      // `/app.js` reads the packaged bundle off disk, and the document
      // describes the PACKAGED surface — so it is in the document on
      // every machine and answerable only where the bundle was built
      // (the same asymmetry openapi-serve found)
      val answerable = obj(get(doc, "paths")).filter((path, _) =>
        path != "/app.js" || okay.chat.Chat.appJs.isDefined)
      answerable.foreach { (path, item) =>
        val media = obj(get(get(get(get(item, "get"), "responses"), "200"), "content")).map(_._1)
        val url = path.replace("{email}", "a@b.c")
        val res = Async.run(routes(Request.get(url))).runWith
        val sent = res.headers.collectFirst {
          case (k, v) if k.equalsIgnoreCase("content-type") => v.takeWhile(_ != ';').trim }
        assertEquals(sent, media.headOption, s"$url: the document says $media")
      }
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
