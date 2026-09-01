package okay.codec

/**
 * The newtype node: to every algebra the wrapper does not exist.
 * Shared suite — the node is pure shape, so JVM, JS and Native all
 * prove it.
 */
class TestIso extends munit.FunSuite {

  final case class UserId(n: Long)
  given Schema[UserId] = Schema.wrap(UserId(_), _.n)

  final case class Port(n: Int)
  given Schema[Port] = Schema.refine(
    (n: Int) => if n >= 1 && n <= 65535 then Right(Port(n)) else Left(s"port $n is out of range"),
    _.n)

  final case class Server(host: String, port: Port, owner: Option[UserId] = None,
                          retries: Int = 3)
  given Schema[Server] = Schema.derived

  test("a wrapped value travels BARE: a string stays a string, a long a number") {
    assertEquals(Json.write(UserId(7)), "7")
    assertEquals(Json.read[UserId]("7"), Right(UserId(7)))
    assertEquals(Cbor.read[UserId](Cbor.write(UserId(7))), Right(UserId(7)))
  }

  test("a product of wrapped fields encodes flat and round-trips both wires") {
    val s = Server("db1", Port(5432), Some(UserId(9)))
    val j = Json.write(s)
    assert(j.contains("\"port\":5432"), j)     // no object, no wrapper
    assert(j.contains("\"owner\":9"), j)
    assertEquals(Json.read[Server](j), Right(s))
    assertEquals(Cbor.read[Server](Cbor.write(s)), Right(s))
  }

  test("refine: a Left is a decode error carrying its message, never a throw") {
    assertEquals(Json.read[Port]("70000"), Left("port 70000 is out of range"))
    assert(Json.read[Server]("""{"host":"h","port":0}""").isLeft)
    assertEquals(Json.read[Port]("5432"), Right(Port(5432)))
  }

  test("wrappers and defaults compose: partial input falls back, then wraps") {
    assertEquals(Json.read[Server]("""{"host":"h","port":80}"""),
      Right(Server("h", Port(80), None, 3)))
  }
}
