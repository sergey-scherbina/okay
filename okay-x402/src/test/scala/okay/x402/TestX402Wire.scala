package okay.x402

import okay.codec.Json
import okay.codec.Json.*

/**
 * Stage 0 goldens: the four headers printed in x402's
 * specs/transports-v2/http.md (coinbase/x402 at 1a79feaf, Apache-2.0),
 * each with the JSON the spec says it decodes to. A header must decode
 * to our types, and our types must encode back to the same JSON (keys
 * in any order; an absent optional stays absent).
 */
class TestX402Wire extends munit.FunSuite:

  private val examples: List[(String, String, String)] = List(
    ("PAYMENT-REQUIRED",
     "eyJ4NDAyVmVyc2lvbiI6MiwiZXJyb3IiOiJQQVlNRU5ULVNJR05BVFVSRSBoZWFkZXIgaXMgcmVxdWlyZWQiLCJyZXNvdXJjZSI6eyJ1cmwiOiJodHRwczovL2FwaS5leGFtcGxlLmNvbS9wcmVtaXVtLWRhdGEiLCJkZXNjcmlwdGlvbiI6IkFjY2VzcyB0byBwcmVtaXVtIG1hcmtldCBkYXRhIiwibWltZVR5cGUiOiJhcHBsaWNhdGlvbi9qc29uIn0sImFjY2VwdHMiOlt7InNjaGVtZSI6ImV4YWN0IiwibmV0d29yayI6ImVpcDE1NTo4NDUzMiIsImFtb3VudCI6IjEwMDAwIiwiYXNzZXQiOiIweDAzNkNiRDUzODQyYzU0MjY2MzRlNzkyOTU0MWVDMjMxOGYzZENGN2UiLCJwYXlUbyI6IjB4MjA5NjkzQmM2YWZjMEM1MzI4YkEzNkZhRjAzQzUxNEVGMzEyMjg3QyIsIm1heFRpbWVvdXRTZWNvbmRzIjo2MCwiZXh0cmEiOnsibmFtZSI6IlVTREMiLCJ2ZXJzaW9uIjoiMiJ9fV19",
     """{"x402Version":2,"error":"PAYMENT-SIGNATURE header is required","resource":{"url":"https://api.example.com/premium-data","description":"Access to premium market data","mimeType":"application/json"},"accepts":[{"scheme":"exact","network":"eip155:84532","amount":"10000","asset":"0x036CbD53842c5426634e7929541eC2318f3dCF7e","payTo":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","maxTimeoutSeconds":60,"extra":{"name":"USDC","version":"2"}}]}"""),
    ("PAYMENT-SIGNATURE",
     "eyJ4NDAyVmVyc2lvbiI6MiwicmVzb3VyY2UiOnsidXJsIjoiaHR0cHM6Ly9hcGkuZXhhbXBsZS5jb20vcHJlbWl1bS1kYXRhIiwiZGVzY3JpcHRpb24iOiJBY2Nlc3MgdG8gcHJlbWl1bSBtYXJrZXQgZGF0YSIsIm1pbWVUeXBlIjoiYXBwbGljYXRpb24vanNvbiJ9LCJhY2NlcHRlZCI6eyJzY2hlbWUiOiJleGFjdCIsIm5ldHdvcmsiOiJlaXAxNTU6ODQ1MzIiLCJhbW91bnQiOiIxMDAwMCIsImFzc2V0IjoiMHgwMzZDYkQ1Mzg0MmM1NDI2NjM0ZTc5Mjk1NDFlQzIzMThmM2RDRjdlIiwicGF5VG8iOiIweDIwOTY5M0JjNmFmYzBDNTMyOGJBMzZGYUYwM0M1MTRFRjMxMjI4N0MiLCJtYXhUaW1lb3V0U2Vjb25kcyI6NjAsImV4dHJhIjp7Im5hbWUiOiJVU0RDIiwidmVyc2lvbiI6IjIifX0sInBheWxvYWQiOnsic2lnbmF0dXJlIjoiMHgyZDZhNzU4OGQ2YWNjYTUwNWNiZjBkOWE0YTIyN2UwYzUyYzZjMzQwMDhjOGU4OTg2YTEyODMyNTk3NjQxNzM2MDhhMmNlNjQ5NjY0MmUzNzdkNmRhOGRiYmY1ODM2ZTliZDE1MDkyZjllY2FiMDVkZWQzZDYyOTNhZjE0OGI1NzFjIiwiYXV0aG9yaXphdGlvbiI6eyJmcm9tIjoiMHg4NTdiMDY1MTlFOTFlM0E1NDUzODc5MWJEYmIwRTIyMzczZTM2YjY2IiwidG8iOiIweDIwOTY5M0JjNmFmYzBDNTMyOGJBMzZGYUYwM0M1MTRFRjMxMjI4N0MiLCJ2YWx1ZSI6IjEwMDAwIiwidmFsaWRBZnRlciI6IjE3NDA2NzIwODkiLCJ2YWxpZEJlZm9yZSI6IjE3NDA2NzIxNTQiLCJub25jZSI6IjB4ZjM3NDY2MTNjMmQ5MjBiNWZkYWJjMDg1NmYyYWViMmQ0Zjg4ZWU2MDM3YjhjYzVkMDRhNzFhNDQ2MmYxMzQ4MCJ9fX0=",
     """{"x402Version":2,"resource":{"url":"https://api.example.com/premium-data","description":"Access to premium market data","mimeType":"application/json"},"accepted":{"scheme":"exact","network":"eip155:84532","amount":"10000","asset":"0x036CbD53842c5426634e7929541eC2318f3dCF7e","payTo":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","maxTimeoutSeconds":60,"extra":{"name":"USDC","version":"2"}},"payload":{"signature":"0x2d6a7588d6acca505cbf0d9a4a227e0c52c6c34008c8e8986a1283259764173608a2ce6496642e377d6da8dbbf5836e9bd15092f9ecab05ded3d6293af148b571c","authorization":{"from":"0x857b06519E91e3A54538791bDbb0E22373e36b66","to":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","value":"10000","validAfter":"1740672089","validBefore":"1740672154","nonce":"0xf3746613c2d920b5fdabc0856f2aeb2d4f88ee6037b8cc5d04a71a4462f13480"}}}"""),
    ("PAYMENT-RESPONSE",
     "eyJzdWNjZXNzIjp0cnVlLCJ0cmFuc2FjdGlvbiI6IjB4MTIzNDU2Nzg5MGFiY2RlZjEyMzQ1Njc4OTBhYmNkZWYxMjM0NTY3ODkwYWJjZGVmMTIzNDU2Nzg5MGFiY2RlZiIsIm5ldHdvcmsiOiJlaXAxNTU6ODQ1MzIiLCJwYXllciI6IjB4ODU3YjA2NTE5RTkxZTNBNTQ1Mzg3OTFiRGJiMEUyMjM3M2UzNmI2NiJ9",
     """{"success":true,"transaction":"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef","network":"eip155:84532","payer":"0x857b06519E91e3A54538791bDbb0E22373e36b66"}"""),
    ("PAYMENT-RESPONSE",
     "eyJzdWNjZXNzIjpmYWxzZSwiZXJyb3JSZWFzb24iOiJpbnN1ZmZpY2llbnRfZnVuZHMiLCJ0cmFuc2FjdGlvbiI6IiIsIm5ldHdvcmsiOiJlaXAxNTU6ODQ1MzIiLCJwYXllciI6IjB4ODU3YjA2NTE5RTkxZTNBNTQ1Mzg3OTFiRGJiMEUyMjM3M2UzNmI2NiJ9",
     """{"success":false,"errorReason":"insufficient_funds","transaction":"","network":"eip155:84532","payer":"0x857b06519E91e3A54538791bDbb0E22373e36b66"}"""))

  /** JSON equality that ignores object key order */
  private def norm(j: Json): Json = j match
    case JObj(fs) => JObj(fs.map((k, v) => k -> norm(v)).sortBy(_._1))
    case JArr(vs) => JArr(vs.map(norm))
    case other => other

  private def roundTrip(header: String, json: Json): Either[String, Json] = header match
    case "PAYMENT-REQUIRED" => X402.paymentRequired(json).map(X402.toJson)
    case "PAYMENT-SIGNATURE" => X402.paymentPayload(json).map(X402.toJson)
    case "PAYMENT-RESPONSE" => X402.settlement(json).map(X402.toJson)
    case other => Left(s"no reader for $other")

  test("every header the spec prints decodes to the JSON the spec says it is") {
    for (h, b64, expected) <- examples do
      assertEquals(X402.unheader(b64).map(norm), Right(norm(Json.parse(expected))), h)
  }

  test("each decodes into the protocol types and encodes back to the same JSON") {
    for (h, b64, expected) <- examples do
      val got = X402.unheader(b64).flatMap(roundTrip(h, _)).map(norm)
      assertEquals(got, Right(norm(Json.parse(expected))), h)
  }

  test("the typed reading: CAIP-2 network, a digit-string amount, scheme-specific parts kept whole") {
    val (_, b64, _) = examples.find(_._1 == "PAYMENT-SIGNATURE").get
    val p = X402.unheader(b64).flatMap(X402.paymentPayload).fold(fail(_), identity)
    assertEquals(p.accepted.network, okay.chain.Network("eip155", "84532"))
    assertEquals(p.accepted.amount, BigInt(10000))
    assert(Json.print(p.payload).contains("authorization"), Json.print(p.payload))
  }

  test("a failed settlement keeps its errorReason; a successful one omits it on the wire") {
    val rs = examples.filter(_._1 == "PAYMENT-RESPONSE").map((_, b, _) => X402.unheader(b).flatMap(X402.settlement).fold(fail(_), identity))
    assertEquals(rs.map(_.success), List(true, false))
    assertEquals(rs(1).errorReason, Some("insufficient_funds"))
    assert(!Json.print(X402.toJson(rs(0))).contains("errorReason"))
  }

  test("refusals name what is wrong: a version, a network, an amount") {
    assert(X402.paymentRequired(Json.parse("""{"x402Version":1,"resource":{"url":"u"},"accepts":[]}""")).left.exists(_.contains("x402Version 1")))
    val bad = """{"scheme":"exact","network":"nope","amount":"1","asset":"a","payTo":"p","maxTimeoutSeconds":60}"""
    assert(X402.requirements(Json.parse(bad)).left.exists(_.contains("CAIP-2")))
    val frac = """{"scheme":"exact","network":"eip155:1","amount":"1.5","asset":"a","payTo":"p","maxTimeoutSeconds":60}"""
    assert(X402.requirements(Json.parse(frac)).left.exists(_.contains("atomic units")))
  }
