package okay.demo

import okay.openapi.{Api, OpenApi}

/**
 * The demo's OpenAPI document as ONE value, and a file rendered from
 * it (specs/openapi.md stage 2).
 *
 * `okay-demo/openapi.json` is this rendering — regenerate with
 * `sbt "okayDemo/runMain okay.demo.DemoOpenApi"`; TestDemoOpenApi
 * refuses a drift between the two, the way TestDemoDeploy does for
 * the deployment.
 *
 * The point of committing it is the reader the spec named: an
 * integrator is not in this repository, so the proxy for them is a
 * diff in review. An API change that nobody meant to make shows up as
 * a changed file next to the code that changed it.
 */
object DemoOpenApi:
  val api: Api = Api(
    title = "okay demo chat",
    version = "1.0",
    description = Some("the demo's HTTP surface, rendered from the router that answers it"))

  /**
   * The document, over the routes this service declares WHEN
   * PACKAGED.
   *
   * `withApp = true` is not a detail: the demo serves `/app.js` only
   * where the linked bundle is present, so the surface varies with
   * the working directory — and a document that varies with the
   * machine cannot be committed or diffed. The drift test found that
   * on its first full run, which is the argument for having one. What
   * is published is therefore what a DEPLOYED service offers, which
   * is also what DemoDeploy's image contains.
   */
  def document(using okay.conf.Secrets, Board, okay.persist.Store): okay.codec.Json =
    OpenApi.document(api, router)

  /** the router the document describes: the packaged surface */
  def router(using okay.conf.Secrets, Board, okay.persist.Store): okay.http.Router =
    ChatDemo.declaredRouter(okay.chat.Chat.scripted, 512, withApp = true)

  def main(args: Array[String]): Unit =
    val store = Board.store(":memory:")
    given okay.persist.Store = store
    given Board = Board(Board.topicOf(store))
    given okay.conf.Secrets = okay.conf.Secrets.memory(Map.empty)
    val path = okay.deploy.Deployment.repoRoot().resolve("okay-demo/openapi.json")
    java.nio.file.Files.writeString(path, okay.codec.Json.print(document) + "\n")
    println(s"wrote $path")
