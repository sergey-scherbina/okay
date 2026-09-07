package okay.acme

import okay.*
import okay.given
import okay.conf.{Secret, Secrets}
import okay.http.{Http, Response}
import okay.jetty.Jetty

/**
 * The providers, by SHAPE: a stub that is the provider for the length
 * of one call — it asserts what we sent and answers what the real one
 * would.
 *
 * No account can be had in a test, and a `Dns` that talks to a real
 * Cloudflare in CI would be a test that fails when someone's card
 * expires. What is worth proving is exactly what a shape test proves:
 * the method, the path, the credential's header, the body's fields,
 * and that a provider's refusal comes back as ITS sentence. The
 * challenge flow around it is already proven for real against Pebble
 * (acme-dns01).
 */
class TestProviders extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val secrets = Secrets.memory(Map("t" -> "sekrit"))

  final case class Seen(method: String, path: String, headers: Map[String, String], body: String)

  /** a server that records what it was sent and answers `reply` */
  private def stub(reply: Int => (Int, String))(body: (String, () => Vector[Seen]) => Unit): Unit =
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Seen]
    Resource.run[Unit, Pure](Jetty.serve(0)({
      case r =>
        val path = r.url
        seen.add(Seen(r.method.name, path,
          r.headers.map((k, v) => k.toLowerCase -> v).toMap, String(r.body.bytes, "UTF-8"))): Unit
        val (status, text) = reply(seen.size)
        pure(Response(status, Vector("Content-Type" -> "application/json"),
          Http.one(text.getBytes("UTF-8"))))
    })().map { server =>
      import scala.jdk.CollectionConverters.*
      body(s"http://127.0.0.1:${Jetty.port(server)}", () => seen.asScala.toVector)
    }).runWith

  test("deSEC: a Token header, a PUT of the rrset, and the delete is the same call with no records") {
    stub(_ => (200, "[]")) { (base, seen) =>
      Resource.run[Unit, Pure](Jetty.http().map { http =>
        val dns = Providers.desec(http, "okay.example", Secret("t"), secrets, endpoint = base)
          .fold(m => fail(m), identity)
        assertEquals(dns.putTxt("_acme-challenge.okay.example", "proof"), Right(()))
        dns.removeTxt("_acme-challenge.okay.example")
      }).runWith
      val calls = seen()
      assertEquals(calls.length, 2)
      assertEquals(calls.head.method, "PUT")
      assertEquals(calls.head.path, "/api/v1/domains/okay.example/rrsets/")
      assertEquals(calls.head.headers.get("authorization"), Some("Token sekrit"))
      // the subname, not the full name; and the QUOTED presentation
      // form, which is what deSEC stores
      assert(calls.head.body.contains("\"subname\":\"_acme-challenge\""), calls.head.body)
      assert(calls.head.body.contains("\\\"proof\\\""), calls.head.body)
      assert(calls.head.body.contains("\"type\":\"TXT\""), calls.head.body)
      // the delete states an EMPTY record set -- the API is declarative
      assert(calls(1).body.contains("\"records\":[]"), calls(1).body)
    }
  }

  test("Cloudflare: a Bearer token, a POST to the zone, and the delete uses the id the create answered") {
    stub(n => if n == 1 then (200, """{"success":true,"result":{"id":"rec-42"}}""") else (200, """{"success":true}""")) {
      (base, seen) =>
        Resource.run[Unit, Pure](Jetty.http().map { http =>
          val dns = Providers.cloudflare(http, "zone-1", Secret("t"), secrets, endpoint = base)
            .fold(m => fail(m), identity)
          assertEquals(dns.putTxt("_acme-challenge.okay.example", "proof"), Right(()))
          dns.removeTxt("_acme-challenge.okay.example")
        }).runWith
        val calls = seen()
        assertEquals(calls.length, 2)
        assertEquals(calls.head.method, "POST")
        assertEquals(calls.head.path, "/client/v4/zones/zone-1/dns_records")
        assertEquals(calls.head.headers.get("authorization"), Some("Bearer sekrit"))
        assert(calls.head.body.contains("\"type\":\"TXT\"") &&
          calls.head.body.contains("\"name\":\"_acme-challenge.okay.example\"") &&
          calls.head.body.contains("\"content\":\"proof\""), calls.head.body)
        // the delete uses the id the create answered -- a zone holds
        // records that are not ours, and a name is not an identity here
        assertEquals(calls(1).method, "DELETE")
        assertEquals(calls(1).path, "/client/v4/zones/zone-1/dns_records/rec-42")
    }
  }

  test("Route 53: signed with the repository's own SigV4, an UPSERT then a DELETE of what went up") {
    stub(_ => (200, "<ChangeResourceRecordSetsResponse/>")) { (base, seen) =>
      Resource.run[Unit, Pure](Jetty.http().map { http =>
        val dns = Providers.route53(http, "Z123", "AKIA", Secret("t"), secrets, endpoint = base)
          .fold(m => fail(m), identity)
        assertEquals(dns.putTxt("_acme-challenge.okay.example", "proof"), Right(()))
        dns.removeTxt("_acme-challenge.okay.example")
      }).runWith
      val calls = seen()
      assertEquals(calls.length, 2)
      assertEquals(calls.head.method, "POST")
      assertEquals(calls.head.path, "/2013-04-01/hostedzone/Z123/rrset/")
      // the signature is the repo's own, and it is THERE
      val auth = calls.head.headers.getOrElse("authorization", "")
      assert(auth.startsWith("AWS4-HMAC-SHA256 Credential=AKIA/"), auth)
      assert(auth.contains("route53/aws4_request"), auth)
      assert(calls.head.headers.contains("x-amz-date"), calls.head.headers.keys.toString)
      assert(calls.head.body.contains("<Action>UPSERT</Action>"), calls.head.body)
      assert(calls.head.body.contains("<Value>\"proof\"</Value>"), calls.head.body)
      assert(calls.head.body.contains("<Name>_acme-challenge.okay.example.</Name>"), calls.head.body)
      // a delete names the record it removes: a zone holds records
      // that are not ours
      assert(calls(1).body.contains("<Action>DELETE</Action>"), calls(1).body)
      assert(calls(1).body.contains("<Value>\"proof\"</Value>"), calls(1).body)
    }
  }

  test("a provider's refusal is ITS sentence, in every dialect they use") {
    stub(_ => (403, """{"errors":[{"code":10000,"message":"Invalid access token"}]}""")) { (base, _) =>
      Resource.run[Unit, Pure](Jetty.http().map { http =>
        val dns = Providers.route53(http, "Z1", "AKIA", Secret("t"), secrets, endpoint = base)
          .fold(m => fail(m), identity)
        val out = dns.putTxt("_acme-challenge.okay.example", "proof")
        assert(out.left.exists(_.contains("Invalid access token")), out.toString)
        assert(out.left.exists(_.contains("403")), out.toString)
      }).runWith
    }
    stub(_ => (400, """{"detail":"invalid subname"}""")) { (base, _) =>
      Resource.run[Unit, Pure](Jetty.http().map { http =>
        val dns = Providers.desec(http, "okay.example", Secret("t"), secrets, endpoint = base)
          .fold(m => fail(m), identity)
        assert(dns.putTxt("x", "y").left.exists(_.contains("invalid subname")))
      }).runWith
    }
  }

  test("a credential is a Secret: a missing one refuses before any request") {
    Resource.run[Unit, Pure](Jetty.http().map { http =>
      assert(Providers.desec(http, "d", Secret("absent"), Secrets.memory(Map.empty)).isLeft)
      assert(Providers.cloudflare(http, "z", Secret("absent"), Secrets.memory(Map.empty)).isLeft)
      assert(Providers.route53(http, "z", "AKIA", Secret("absent"), Secrets.memory(Map.empty)).isLeft)
    }).runWith
  }
