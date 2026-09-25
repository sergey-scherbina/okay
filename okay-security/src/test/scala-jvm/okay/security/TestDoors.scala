package okay.security

import okay.{!, Async, provide, pure}
import okay.given
import okay.http.{Http, Request, Response}

/** each door delegates to its explicit form — one assertion per
 * door, against stubs where a wire would be */
class TestDoors extends munit.FunSuite {

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** an Http stub answering a fixed response — the wire, removed */
  def stub(status: Int, body: String): Http = new Http:
    def send(r: Request): Response ! Async =
      pure(Response(status, Nil, okay.http.Http.one(body.getBytes("UTF-8"))))

  val client = OAuth2.Client("cid", None, "http://as/a", "http://as/t", "http://app/cb", Nil)

  test("OAuth2 doors: exchange/refresh/clientCredentials under an ambient Http") {
    val ok = stub(200, """{"access_token":"tok","token_type":"Bearer"}""")
    provide(ok) {
      assertEquals(run(OAuth2.exchange(client, "code", "ver")).map(_.access), Right("tok"))
      assertEquals(run(OAuth2.refresh(client, "r")).map(_.access), Right("tok"))
      assertEquals(run(OAuth2.clientCredentials(client)).map(_.access), Right("tok"))
    }
    // and the refusal path flows the same
    provide(stub(400, """{"error":"invalid_grant"}""")) {
      assert(run(OAuth2.exchange(client, "code", "ver")).isLeft)
    }
  }

  test("Jwks door: fetch under an ambient Http parses the set") {
    provide(stub(200, """{"keys":[]}""")) {
      assertEquals(run(Jwks.fetch("http://issuer/jwks")), Map.empty)
    }
  }

}
