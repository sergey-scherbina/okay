package okay.x402.mcp

import okay.*
import okay.given
import okay.codec.Json
import okay.codec.Json.*
import okay.mcp.{Rpc, Server}
import okay.x402.*
import scala.concurrent.Future
import Fixtures.*

/** the gate as a pure stage: messages in, messages out, a counting
 * facilitator — every road of stage 3's server side */
class TestX402McpGate extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private def drive(t: Tools, around: Server.Around[Async], msgs: Rpc*): Future[Seq[Rpc]] =
    Async.runAsync(Writer.run[Rpc, Unit, Async](through[Rpc, Rpc, Async, Unit, Unit](
      okay.Source.of((hello +: msgs).toList))(
      Server.serveIn[Async](t.serving, around)(Server.answering(t.serving)))).map(_._1.drop(1)))

  private def refusal(r: Rpc): (Int, String, Option[PaymentRequired]) = r match
    case Rpc.Failed(_, code, message, data) => (code, message, data.flatMap(X402.paymentRequired(_).toOption))
    case other => fail(s"not a refusal: $other")

  private def answer(r: Rpc): Json = r match
    case Rpc.Answer(_, result) => result
    case other => fail(s"not an answer: $other")

  test("unpaid: a JSON-RPC 402 whose data is the PaymentRequired; the tool did not run; a free tool is untouched") {
    val t = Tools()
    drive(t, X402Mcp.gate(prices, Counting()), call(1, "analysis"), call(2, "free")).map { out =>
      val (code, message, q) = refusal(out(0))
      assertEquals((code, message), (402, "Payment required to access this resource"))
      assertEquals(q.map(_.accepts), Some(Vector(price)))
      assertEquals(q.map(_.resource.url), Some("mcp://tool/analysis"))
      assertEquals(t.ran, 0)
      assert(Json.print(answer(out(1))).contains("gratis"), out(1).toString)
    }
  }

  test("paid: the tool runs once, is settled once, and the receipt rides in result._meta") {
    val t = Tools(); val f = Counting()
    drive(t, X402Mcp.gate(prices, f), call(1, "analysis", Some(payment("good")))).map { out =>
      val result = answer(out(0))
      assert(Json.print(result).contains("strong fundamentals"), Json.print(result))
      assertEquals(X402Mcp.receipt(result).map(s => (s.success, s.transaction)), Some((true, "0xtx")))
      assertEquals((t.ran, f.settles), (1, 1))
    }
  }

  test("a replayed payment is refused with 402 and neither runs nor settles again") {
    val t = Tools(); val f = Counting()
    val p = payment("good")
    drive(t, X402Mcp.gate(prices, f), call(1, "analysis", Some(p)), call(2, "analysis", Some(p))).map { out =>
      assertEquals(refusal(out(1))._1, 402)
      assertEquals(refusal(out(1))._3.flatMap(_.error), Some("this payment was already used"))
      assertEquals((t.ran, f.settles), (1, 1))
    }
  }

  test("a payment that does not verify is a 402 naming the facilitator's reason") {
    val t = Tools()
    drive(t, X402Mcp.gate(prices, Counting()), call(1, "analysis", Some(payment("forged")))).map { out =>
      assertEquals(refusal(out(0))._2, "payment invalid: invalid_signature")
      assertEquals(t.ran, 0)
    }
  }

  test("a malformed payment is invalid params (-32602), not a price") {
    val t = Tools()
    val params = X402Mcp.withMeta(call(1, "analysis").params, X402Mcp.Payment, JStr("not a payload"))
    drive(t, X402Mcp.gate(prices, Counting()), Rpc.Request(JNum(1), "tools/call", params)).map { out =>
      assertEquals(refusal(out(0))._1, Rpc.InvalidParams)
      assertEquals(t.ran, 0)
    }
  }

  test("a tool that fails (isError) is not charged, and the same payment is good again") {
    val t = Tools(); val f = Counting()
    val p = payment("good")
    drive(t, X402Mcp.gate(prices, f), call(1, "broken", Some(p)), call(2, "analysis", Some(p))).map { out =>
      assertEquals(X402Mcp.receipt(answer(out(0))), None)
      assertEquals(X402Mcp.receipt(answer(out(1))).map(_.success), Some(true))
      assertEquals((t.ran, f.settles), (2, 1))
    }
  }

  test("a failed settlement is a 402 carrying the failed settlement beside the requirements, and releases the payment") {
    val t = Tools(); val f = Counting(settleOk = false)
    val p = payment("good")
    drive(t, X402Mcp.gate(prices, f), call(1, "analysis", Some(p)), call(2, "analysis", Some(p))).map { out =>
      out(0) match
        case Rpc.Failed(_, code, message, data) =>
          assertEquals((code, message), (402, "Payment settlement failed: insufficient_funds"))
          assertEquals(data.flatMap(X402.paymentRequired(_).toOption).map(_.accepts), Some(Vector(price)))
          val s = data.flatMap(Rpc.field(_, X402Mcp.PaymentResponse)).flatMap(X402.settlement(_).toOption)
          assertEquals(s.flatMap(_.errorReason), Some("insufficient_funds"))
        case other => fail(s"not a refusal: $other")
      // released: the second try is admitted again (and fails to settle again)
      assertEquals(refusal(out(1))._2, "Payment settlement failed: insufficient_funds")
      assertEquals(f.settles, 2)
    }
  }

  test("Around.none changes nothing: the same answers as serveIn without one") {
    val t = Tools()
    drive(t, Server.Around.none[Async], call(1, "analysis")).map { out =>
      assert(Json.print(answer(out(0))).contains("strong fundamentals"))
    }
  }
