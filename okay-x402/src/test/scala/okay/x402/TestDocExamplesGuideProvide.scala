package okay.x402

import okay.*
import okay.conf.Secrets
import okay.http.{Http, Response}

/**
 * docs/guide.md, "provide" and "providing", VERBATIM
 * (doc-snippet-debt). Here because this module sees both of the
 * page's capabilities, okay-http's `Http` and okay-conf's `Secrets`.
 * The program `app` needs both and names neither's implementation.
 */
class TestDocExamplesGuideProvide extends munit.FunSuite:

  def fixed: Http = _ => okay.pure(Response(200, Nil, Http.one(Array.emptyByteArray)))
  val prodHttp: Http = fixed
  val stubHttp: Http = fixed
  val testSecrets: Secrets = Secrets.memory(Map.empty)

  /** the program: it answers the capabilities it was handed, so the
   * test can see WHICH ones were installed */
  def app(using http: Http, secrets: Secrets): (Http, Secrets) = (http, secrets)

  def same(got: (Http, Secrets), http: Http, secrets: Secrets): Boolean =
    (got._1 eq http) && (got._2 eq secrets)

  test("provide: the edge and the test run the same program") {
    val edge =
      provide(prodHttp, Secrets.env) { app }     // the edge
    val inTest =
      provide(stubHttp, testSecrets) { app }     // the test — same program
    assert(same(edge, prodHttp, Secrets.env))
    assert(same(inTest, stubHttp, testSecrets))
  }

  test("providing: a base environment, one layer overridden") {
    val base = providing[Http](prodHttp) and providing[Secrets](Secrets.env)
    val atEdge =
      base { app }                                        // the edge
    val overridden =
      (base and providing[Http](stubHttp)) { app }        // override just Http
    assert(same(atEdge, prodHttp, Secrets.env))
    assert(same(overridden, stubHttp, Secrets.env), "the override replaced more than Http")
  }
