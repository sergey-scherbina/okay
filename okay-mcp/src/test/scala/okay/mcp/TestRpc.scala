package okay.mcp

import okay.*
import okay.given
import okay.codec.Json

/** The wire: JSON-RPC as data, and decoding that cannot throw. */
class TestRpc extends munit.FunSuite {

  private val q = "\""

  test("the four shapes decode by the fields they carry") {
    assertEquals(
      Rpc.decode(s"{${q}jsonrpc$q:${q}2.0$q,${q}id$q:1,${q}method$q:${q}tools/list$q}"),
      Rpc.Request(Json.JNum(1), "tools/list", Json.JObj(Vector.empty)))
    assertEquals(
      Rpc.decode(s"{${q}jsonrpc$q:${q}2.0$q,${q}method$q:${q}notifications/initialized$q}"),
      Rpc.Notify("notifications/initialized", Json.JObj(Vector.empty)))
    assertEquals(
      Rpc.decode(s"{${q}jsonrpc$q:${q}2.0$q,${q}id$q:2,${q}result$q:{${q}ok$q:true}}"),
      Rpc.Answer(Json.JNum(2), Json.JObj(Vector("ok" -> Json.JBool(true)))))
    assertEquals(
      Rpc.decode(s"{${q}jsonrpc$q:${q}2.0$q,${q}id$q:3,${q}error$q:{${q}code$q:-32601,${q}message$q:${q}nope$q}}"),
      Rpc.Failed(Json.JNum(3), Rpc.MethodNotFound, "nope"))
  }

  test("encode and decode round-trip, all four") {
    val ms = Seq(
      Rpc.Request(Json.JStr("a"), "ping", Json.JObj(Vector.empty)),
      Rpc.Notify("x/y", Json.JObj(Vector("k" -> Json.JNum(1)))),
      Rpc.Answer(Json.JNum(7), Json.JArr(Vector(Json.JStr("s")))),
      Rpc.Failed(Json.JNull, Rpc.ParseError, "damaged"))
    for m <- ms do assertEquals(Rpc.decode(Rpc.encode(m)), m)
  }

  test("damage is a VALUE: no line can throw, and the stream continues") {
    for bad <- Seq("", "{", "not json at all", "[1,2,3]", s"{${q}jsonrpc$q:${q}2.0$q}",
                   s"{${q}id$q:1}", " ") do
      Rpc.decode(bad) match
        case Rpc.Failed(_, code, _) =>
          assert(code == Rpc.ParseError || code == Rpc.InvalidRequest, s"$bad -> $code")
        case other => fail(s"'$bad' decoded to $other")
  }

  test("a parse error answers with id null, which is what JSON-RPC owes") {
    val Rpc.Failed(id, code, _) = Rpc.decode("{oops"): @unchecked
    assertEquals(id, Json.JNull)
    assertEquals(code, Rpc.ParseError)
  }

  test("the framing stage: lines in, messages out, blank lines skipped") {
    val lines = List(
      s"{${q}jsonrpc$q:${q}2.0$q,${q}id$q:1,${q}method$q:${q}ping$q}",
      "",
      "   ",
      "garbage",
      s"{${q}jsonrpc$q:${q}2.0$q,${q}id$q:2,${q}result$q:{}}")
    val (out, _) = !.run(Writer.run(through(Writer.of(lines))(Rpc.messages)))
    assertEquals(out.length, 3)   // two good, one damaged; the blanks are framing
    assertEquals(out(0), Rpc.Request(Json.JNum(1), "ping", Json.JObj(Vector.empty)))
    assert(out(1).isInstanceOf[Rpc.Failed], out(1).toString)
    assertEquals(out(2), Rpc.Answer(Json.JNum(2), Json.JObj(Vector.empty)))
  }
}
