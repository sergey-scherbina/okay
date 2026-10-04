package okay2.persist

import okay2.codec.{Cbor, Schema}

/**
 * The typed view (okay-persist's Typed.scala): bytes stay in the engine,
 * Schema/CBOR live here at the edge, and damage is DATA — a record that
 * does not decode names its offset and its error instead of throwing.
 *
 * Journal-grade topics carry an ENVELOPE: a four-byte big-endian version
 * before the CBOR payload. Readers upcast old versions through pure
 * `v -> v+1` byte-level steps; a version the reader does not know is an
 * explicit error value at the exact record, not a crash.
 */
final class Typed[A](val topic: Topic, version: Int, upcasts: Map[Int, Typed.Upcast])(implicit s: Schema[A]) {

  def append(partition: Int, key: Array[Byte], a: A, ack: Ack): Long =
    topic.append(partition, key, Typed.seal(version, Cbor.write(a)), ack)

  /** keyed convenience, routing as the raw topic does */
  def append(key: Array[Byte], a: A, ack: Ack = Ack.Durable): Long =
    topic.append(key, Typed.seal(version, Cbor.write(a)), ack)

  def read(partition: Int, from: Long, max: Int): Typed.Read[A] =
    topic.read(partition, from, max) match {
      case Topic.Read.TooEarly(b) => Typed.Read.TooEarly(b)
      case Topic.Read.Records(rs) => Typed.Read.Records(rs.map(decode))
    }

  /** total: the envelope, the upcast chain, then the Schema — each
   * failure an answer naming the offset */
  def decode(r: Record): Typed.Decoded[A] =
    Typed.open(r.value) match {
      case None => Typed.Decoded.Bad(r.offset, "no envelope: value shorter than the version prefix")
      case Some((v, payload)) =>
        if (v > version)
          Typed.Decoded.Bad(r.offset, s"version $v at offset ${r.offset}: this reader knows up to $version")
        else {
          var cur = v
          var bytes: Either[String, Array[Byte]] = Right(payload)
          while (cur < version && bytes.isRight) {
            upcasts.get(cur) match {
              case None => bytes = Left(s"version $cur at offset ${r.offset}: no upcast to ${cur + 1}")
              case Some(up) => bytes = bytes.flatMap(up); cur += 1
            }
          }
          bytes.flatMap(Cbor.read[A](_)) match {
            case Right(a) => Typed.Decoded.Ok(r.offset, r.timestamp, r.key, a)
            case Left(e) => Typed.Decoded.Bad(r.offset, e)
          }
        }
    }
}

object Typed {

  def apply[A](topic: Topic, version: Int = 1, upcasts: Map[Int, Upcast] = Map.empty)(implicit s: Schema[A]): Typed[A] =
    new Typed[A](topic, version, upcasts)

  /** one evolution step: payload bytes at version v to payload bytes at
   * version v + 1 */
  type Upcast = Array[Byte] => Either[String, Array[Byte]]

  /** lift a pure `Old => New` over two Schemas into a byte-level step */
  def step[Old, New](f: Old => New)(implicit o: Schema[Old], n: Schema[New]): Upcast =
    bs => Cbor.read[Old](bs).map(x => Cbor.write(f(x)))

  sealed trait Read[+A]

  object Read {
    final case class Records[+A](records: Vector[Decoded[A]]) extends Read[A]
    final case class TooEarly(begin: Long) extends Read[Nothing]
  }

  sealed trait Decoded[+A]

  object Decoded {
    final case class Ok[+A](offset: Long, timestamp: Long, key: Array[Byte], value: A) extends Decoded[A]
    final case class Bad(offset: Long, error: String) extends Decoded[Nothing]
  }

  private[persist] def seal(version: Int, payload: Array[Byte]): Array[Byte] = {
    val out = new Array[Byte](4 + payload.length)
    out(0) = (version >> 24).toByte
    out(1) = (version >> 16).toByte
    out(2) = (version >> 8).toByte
    out(3) = version.toByte
    System.arraycopy(payload, 0, out, 4, payload.length)
    out
  }

  private[persist] def open(value: Array[Byte]): Option[(Int, Array[Byte])] =
    if (value.length < 4) None
    else {
      val v = ((value(0) & 0xff) << 24) | ((value(1) & 0xff) << 16) |
        ((value(2) & 0xff) << 8) | (value(3) & 0xff)
      Some((v, java.util.Arrays.copyOfRange(value, 4, value.length)))
    }

  /** the spec's `Topic.of[A]`: the typed view over a raw topic */
  implicit final class TopicOf(private val t: Topic) extends AnyVal {
    def of[A](version: Int = 1, upcasts: Map[Int, Upcast] = Map.empty)(implicit s: Schema[A]): Typed[A] =
      new Typed[A](t, version, upcasts)
  }
}
