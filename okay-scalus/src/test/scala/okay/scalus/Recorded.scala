package okay.scalus

import okay.codec.Json

/**
 * A real preprod relay session (src/test/resources/n2n/preprod-session.json),
 * recorded by the independent Python probe beside it (record.py, cbor2 +
 * hashlib): handshake, an intersection five blocks back, five headers,
 * one range fetch of their bodies. Every expected number below was
 * derived twice outside this code — by the probe and by Koios.
 */
object Recorded:
  final case class Seg(out: Boolean, header: Array[Byte], payload: Array[Byte])

  private def unhex(s: String) = Header.unhex(s)

  lazy val json: Json =
    val src = scala.io.Source.fromResource("n2n/preprod-session.json")
    try Json.parse(src.mkString) finally src.close()

  private def field(j: Json, k: String): Json = j match
    case Json.JObj(fs) => fs.collectFirst { case (`k`, v) => v }.getOrElse(Json.JNull)
    case _ => Json.JNull

  lazy val segments: Vector[Seg] = field(json, "segments") match
    case Json.JArr(vs) => vs.collect { case Json.JArr(Vector(Json.JStr(d), Json.JStr(h), Json.JStr(p))) =>
      Seg(d == "out", unhex(h), unhex(p)) }
    case other => sys.error(s"no segments: $other")

  def inbound: Vector[N2N.Segment] = segments.filterNot(_.out).map { s =>
    val (t, r, p, _) = N2N.Segment.header(s.header)
    N2N.Segment(t, r, p, s.payload)
  }
  def outbound: Vector[Seg] = segments.filter(_.out)

  /** the intersection point the probe asked for */
  lazy val intersect: Checkpoint =
    val o = field(json, "intersect")
    (field(o, "hash"), field(o, "abs_slot"), field(o, "block_height")) match
      case (Json.JStr(h), Json.JNum(slot), Json.JNum(no)) => Checkpoint(slot.toLong, h, no.toLong)
      case other => sys.error(s"no intersect: $other")

  // ---- oracles: Koios and the Python probe agree on every one --------

  val blockNos: Vector[Long] = (5209703L to 5209707L).toVector
  val hashes: Vector[String] = Vector(
    "415673333095a8845dee308daca1c83ff8bd9f48e9b208fffccb449b774e6bd2",
    "83953731a57456460129c345b8c4a244ed7ed3780ef83ed7eea4e3f1097242eb",
    "baae000a1b8307a8fac1e7f31f8371ec38fdd75ff5e0881527bbebb761d8d902",
    "926cbd24152822a7b617a1f1ba08dea1ac45b9e2b9ab48cd6c3323a311bdae82",
    "13eab1f3d6b061bc80786b330c30a75c246860292a646e15173aaf7244ac7e28")
  val txCounts: Vector[Int] = Vector(2, 0, 0, 2, 0)
  /** (tx id, fee, inputs, outputs) of the two blocks that carry any */
  val txs: Map[Int, Vector[(String, Long, Int, Int)]] = Map(
    0 -> Vector(("c7734f706415fbe588155c09f44550d2864c38da4f3d40fa8924d26f013f4c8f", 180153L, 3, 3),
                ("a1f93cd271a224c807219c4dcb0147eea902ed7a1c73acf7d4d6695bb2960c3c", 206773L, 1, 2)),
    3 -> Vector(("a04551a17b67040634f09b311196461f983ccb4ac829d07135eaa4d60a50a0f9", 196873L, 5, 5),
                ("677a0f7b9b11354395323c903573897e941ec52a3cbf84483071a97b25de3af4", 251477L, 1, 1)))

  /** a wire that plays the recorded inbound segments back, in order, and
   * keeps what the client wrote */
  final class Replay extends Wire:
    private var in = inbound
    val written = scala.collection.mutable.ArrayBuffer.empty[N2N.Segment]
    def read(): Wire.Read = in match
      case s +: rest => in = rest; Wire.Read.Got(s)
      case _ => Wire.Read.Closed
    def write(s: N2N.Segment): Unit = written += s
    def close(): Unit = ()
