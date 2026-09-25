package okay2.codec

final case class UserId(n: Long)
object UserId {
  implicit val schema: Schema[UserId] = Schema.wrap(UserId(_), _.n)
}
final case class Port(n: Int)
object Port {
  implicit val schema: Schema[Port] = Schema.refine(
    (n: Int) => if (n >= 1 && n <= 65535) Right(Port(n)) else Left(s"port $n is out of range"),
    (p: Port) => p.n)
}
final case class Server(host: String, port: Port, owner: Option[UserId] = None, retries: Int = 3)

/** The newtype node: to every algebra the wrapper does not exist
 * (okay-codec's TestIso, JSON half). A companion's instance wins over
 * derivation, which would make `UserId` an object. */
class TestIso extends munit.FunSuite {

  test("a wrapped value travels BARE") {
    assertEquals(Json.write(UserId(7)), "7")
    assertEquals(Json.read[UserId]("7"), Right(UserId(7)))
  }

  test("a product of wrapped fields encodes flat and round-trips") {
    val s = Server("db1", Port(5432), Some(UserId(9)))
    val j = Json.write(s)
    assert(j.contains("\"port\":5432"), j)
    assert(j.contains("\"owner\":9"), j)
    assertEquals(Json.read[Server](j), Right(s))
    assertEquals(Json.readStrict[Server](j), Right(s))
  }

  test("refine: a Left is a decode error carrying its message, never a throw") {
    assertEquals(Json.read[Port]("70000"), Left("port 70000 is out of range"))
    assert(Json.read[Server]("""{"host":"h","port":0}""").isLeft)
    assertEquals(Json.read[Port]("5432"), Right(Port(5432)))
  }

  test("wrappers and defaults compose: partial input falls back, then wraps") {
    assertEquals(Json.read[Server]("""{"host":"h","port":80}"""), Right(Server("h", Port(80), None, 3)))
  }
}
