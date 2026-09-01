package okay.persist

import okay.codec.{Cbor, Schema}

/**
 * The typed view (specs/persist.md, Interface and Evolution): bytes
 * stay in the engine, Schema/CBOR live here at the edge, and damage
 * is DATA — a record that does not decode names its offset and its
 * error instead of throwing, so one bad record cannot take down a
 * refold.
 *
 * Journal-grade topics carry an ENVELOPE: a four-byte big-endian
 * version before the CBOR payload. Readers upcast old versions
 * through pure `v -> v+1` byte-level steps at decode; a version the
 * reader does not know — newer than it, or older with no step — is
 * an explicit error value at the exact record, not a crash (the
 * Durable fingerprint lesson: drift is caught loudly, not fed to
 * the wrong code).
 */
final class Typed[A](val topic: Topic, version: Int,
                     upcasts: Map[Int, Typed.Upcast])(using Schema[A]):

  def append(partition: Int, key: Array[Byte], a: A, ack: Ack): Long =
    topic.append(partition, key, Typed.seal(version, Cbor.write(a)), ack)

  /** keyed convenience, routing as the raw topic does */
  def append(key: Array[Byte], a: A, ack: Ack = Ack.Durable): Long =
    topic.append(key, Typed.seal(version, Cbor.write(a)), ack)

  def read(partition: Int, from: Long, max: Int): Typed.Read[A] =
    topic.read(partition, from, max) match
      case Topic.Read.TooEarly(b) => Typed.Read.TooEarly(b)
      case Topic.Read.Records(rs) => Typed.Read.Records(rs.map(decode))

  /** total: the envelope, the upcast chain, then the Schema — each
   * failure an answer naming the offset */
  def decode(r: Record): Typed.Decoded[A] =
    Typed.open(r.value) match
      case None => Typed.Decoded.Bad(r.offset, "no envelope: value shorter than the version prefix")
      case Some((v, payload)) =>
        if v > version then
          Typed.Decoded.Bad(r.offset, s"version $v at offset ${r.offset}: this reader knows up to $version")
        else
          var cur = v
          var bytes: Either[String, Array[Byte]] = Right(payload)
          while cur < version && bytes.isRight do
            upcasts.get(cur) match
              case None => bytes = Left(s"version $cur at offset ${r.offset}: no upcast to ${cur + 1}")
              case Some(up) => bytes = up(bytes.toOption.get); cur += 1
          bytes.flatMap(Cbor.read[A](_)) match
            case Right(a) => Typed.Decoded.Ok(r.offset, r.timestamp, r.key, a)
            case Left(e) => Typed.Decoded.Bad(r.offset, e)

object Typed:

  /** one evolution step: payload bytes at version v to payload bytes
   * at version v + 1 */
  type Upcast = Array[Byte] => Either[String, Array[Byte]]

  /** lift a pure `Old => New` over two Schemas into a byte-level step */
  def step[Old, New](f: Old => New)(using Schema[Old], Schema[New]): Upcast =
    bs => Cbor.read[Old](bs).map(o => Cbor.write(f(o)))

  enum Read[+A]:
    case Records(records: Vector[Decoded[A]])
    case TooEarly(begin: Long)

  enum Decoded[+A]:
    case Ok(offset: Long, timestamp: Long, key: Array[Byte], value: A)
    case Bad(offset: Long, error: String)

  private[persist] def seal(version: Int, payload: Array[Byte]): Array[Byte] =
    val out = new Array[Byte](4 + payload.length)
    out(0) = (version >> 24).toByte
    out(1) = (version >> 16).toByte
    out(2) = (version >> 8).toByte
    out(3) = version.toByte
    System.arraycopy(payload, 0, out, 4, payload.length)
    out

  private[persist] def open(value: Array[Byte]): Option[(Int, Array[Byte])] =
    if value.length < 4 then None
    else
      val v = ((value(0) & 0xff) << 24) | ((value(1) & 0xff) << 16) |
        ((value(2) & 0xff) << 8) | (value(3) & 0xff)
      Some((v, java.util.Arrays.copyOfRange(value, 4, value.length)))

/** the spec's `Topic.of[A]`: the typed view over a raw topic */
extension (t: Topic)
  def of[A](version: Int = 1, upcasts: Map[Int, Typed.Upcast] = Map.empty)
           (using Schema[A]): Typed[A] =
    Typed[A](t, version, upcasts)
