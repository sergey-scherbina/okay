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

  /** the document's keys are lowercase verbs; a test that drove every
   * path with GET passed only because every operation WAS a GET, and
   * said so the moment one was not (demo-admin-declared) */
  private def verb(name: String): okay.http.Method = name match
    case "get" => okay.http.Method.Get
    case "post" => okay.http.Method.Post
    case "put" => okay.http.Method.Put
    case "patch" => okay.http.Method.Patch
    case "delete" => okay.http.Method.Delete
    case other => fail(s"the document names a method the test cannot drive: $other")

  private def obj(j: Json): Vector[(String, Json)] = j match
    case Json.JObj(fs) => fs
    case _ => Vector.empty
  private def get(j: Json, k: String): Json =
    obj(j).collectFirst { case (`k`, v) => v }.getOrElse(Json.JNull)

  /** every (path, method) the document names, with its operation */
  private def operations(doc: Json): Vector[(String, String, Json)] =
    obj(get(doc, "paths")).flatMap((path, item) => obj(item).map((m, op) => (path, m, op)))

  test("what the document says an operation answers is what the service sends") {
    // the law of openapi-media on a REAL service: drive each operation
    // and compare the content-type on the wire with the media the
    // document files THAT STATUS under. Comparing against 200 only
    // worked while nothing answered anything else; a secured operation
    // answers 401 with no body, and the document says no body.
    withCaps {
      val routes = DemoOpenApi.router.routes
      val doc = DemoOpenApi.document
      // `/app.js` reads the packaged bundle off disk, and the document
      // describes the PACKAGED surface — so it is in the document on
      // every machine and answerable only where the bundle was built
      // (the same asymmetry openapi-serve found)
      val answerable = operations(doc).filter((path, _, _) =>
        path != "/app.js" || okay.chat.Chat.appJs.isDefined)
      answerable.foreach { (path, m, op) =>
        val url = path.replace("{email}", "a@b.c")
        val res = Async.run(routes(okay.http.Request(verb(m), url, Nil))).runWith
        val media = obj(get(get(get(op, "responses"), res.status.toString), "content")).map(_._1)
        val sent = res.headers.collectFirst {
          case (k, v) if k.equalsIgnoreCase("content-type") => v.takeWhile(_ != ';').trim }
        assertEquals(sent, media.headOption,
          s"$m $url answered ${res.status}: the document files it under $media")
      }
    }
  }

  test("every operation the document names is one the router dispatches") {
    withCaps {
      val routes = DemoOpenApi.router.routes
      val ops = operations(DemoOpenApi.document)
      assert(ops.nonEmpty)
      ops.foreach { (path, m, _) =>
        val url = path.replace("{email}", "a@b.c")
        assert(routes.isDefinedAt(okay.http.Request(verb(m), url, Nil)),
          s"the document names $m $url and the router does not dispatch it")
      }
    }
  }

  test("a protected operation is one the router REFUSES, and the document says which") {
    // the set equality of specs/route-headers.md stage B, on a real
    // service: what the document marks `security` is what the table
    // will not serve without a credential
    withCaps {
      val doc = DemoOpenApi.document
      val marked = operations(doc).collect {
        case (path, m, op) if get(op, "security") != Json.JNull => (path, m) }.toSet
      assertEquals(marked, Set(("/admin/replay", "post")))
      val routes = DemoOpenApi.router.routes
      marked.foreach { (path, m) =>
        val res = Async.run(routes(okay.http.Request(verb(m), path, Nil))).runWith
        assertEquals(res.status, 401, s"$m $path is marked protected and answered ${res.status}")
      }
    }
  }
