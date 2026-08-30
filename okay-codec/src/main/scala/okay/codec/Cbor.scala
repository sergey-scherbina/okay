package okay.codec

import scala.collection.mutable.ArrayBuffer

/**
 * CBOR (RFC 8949) as the second algebra over the SAME Schema: what
 * JSON renders as text, CBOR renders as typed binary items — one
 * derived Schema serves both, which is the cross-format contract of
 * specs/codecs.md. Products are maps keyed by field names, sums are
 * one-entry maps keyed by case name, None is null — the same
 * semantic content as the JSON dialect, so the two decode to equal
 * values. Decoding is total in the value: errors come back as Left,
 * never as a throw.
 */
object Cbor {

  // ---------------------------------------------------------------- encode

  private def header(out: ArrayBuffer[Byte], major: Int, n: Long): Unit =
    val m = major << 5
    if n < 24 then out += (m | n.toInt).toByte
    else if n < 256 then { out += (m | 24).toByte; out += n.toByte }
    else if n < 65536 then
      out += (m | 25).toByte
      out += (n >> 8).toByte; out += n.toByte
    else if n < (1L << 32) then
      out += (m | 26).toByte
      var i = 24
      while i >= 0 do { out += (n >> i).toByte; i -= 8 }
    else
      out += (m | 27).toByte
      var i = 56
      while i >= 0 do { out += (n >> i).toByte; i -= 8 }

  private def integer(out: ArrayBuffer[Byte], n: Long): Unit =
    if n >= 0 then header(out, 0, n) else header(out, 1, -1 - n)

  private def text(out: ArrayBuffer[Byte], s: String): Unit =
    val bs = s.getBytes("UTF-8")
    header(out, 3, bs.length.toLong)
    out ++= bs

  private def put[A](out: ArrayBuffer[Byte], s: Schema[A], a: A): Unit = s match
    case Schema.SInt => integer(out, a.asInstanceOf[Int].toLong)
    case Schema.SLong => integer(out, a.asInstanceOf[Long])
    case Schema.SDouble =>
      out += 0xFB.toByte
      val bits = java.lang.Double.doubleToLongBits(a.asInstanceOf[Double])
      var i = 56
      while i >= 0 do { out += (bits >> i).toByte; i -= 8 }
    case Schema.SBool =>
      out += (if a.asInstanceOf[Boolean] then 0xF5 else 0xF4).toByte
    case Schema.SString => text(out, a.asInstanceOf[String])
    case Schema.SOption(of) => a.asInstanceOf[Option[Any]] match
      case None => out += 0xF6.toByte
      case Some(x) => put(out, of().asInstanceOf[Schema[Any]], x)
    case Schema.SList(of) =>
      val xs = a.asInstanceOf[List[Any]]
      header(out, 4, xs.length.toLong)
      xs.foreach(put(out, of().asInstanceOf[Schema[Any]], _))
    case p: Schema.SProduct[A] =>
      header(out, 5, p.fields.length.toLong)
      p.parts(a).zip(p.fields).foreach { (v, f) =>
        text(out, f._1)
        put(out, f._2().asInstanceOf[Schema[Any]], v)
      }
    case su: Schema.SSum[A] =>
      header(out, 5, 1)
      val (name, sc) = su.cases(su.caseOf(a))
      text(out, name)
      put(out, sc().asInstanceOf[Schema[Any]], a)

  /** value to bytes in one move */
  def write[A](a: A)(using s: Schema[A]): Array[Byte] =
    val out = ArrayBuffer[Byte]()
    put(out, s, a)
    out.toArray

  // ---------------------------------------------------------------- decode

  private final class In(bs: Array[Byte]):
    private var i = 0
    def peek: Int = if i < bs.length then bs(i) & 0xFF else -1
    def byte(): Either[String, Int] =
      if i < bs.length then { val b = bs(i) & 0xFF; i += 1; Right(b) }
      else Left("truncated CBOR")
    def take(n: Int): Either[String, Array[Byte]] =
      if n >= 0 && i + n <= bs.length then { val a = bs.slice(i, i + n); i += n; Right(a) }
      else Left("truncated CBOR")
    def long(n: Int): Either[String, Long] =
      take(n).map(_.foldLeft(0L)((acc, b) => (acc << 8) | (b & 0xFF)))

  private def head(in: In): Either[String, (Int, Long)] =
    in.byte().flatMap { b =>
      val major = b >> 5
      (b & 0x1F) match
        case n if n < 24 => Right((major, n.toLong))
        case 24 => in.long(1).map((major, _))
        case 25 => in.long(2).map((major, _))
        case 26 => in.long(4).map((major, _))
        case 27 => in.long(8).map((major, _))
        case x => Left(s"unsupported additional info $x")
    }

  private def intItem(in: In): Either[String, Long] =
    head(in).flatMap {
      case (0, n) => Right(n)
      case (1, n) => Right(-1 - n)
      case (m, _) => Left(s"expected an integer, got major $m")
    }

  private def textItem(in: In): Either[String, String] =
    head(in).flatMap {
      case (3, n) => in.take(n.toInt).map(String(_, "UTF-8"))
      case (m, _) => Left(s"expected a text string, got major $m")
    }

  private def get[A](in: In, s: Schema[A]): Either[String, A] = s match
    case Schema.SInt => intItem(in).map(_.toInt.asInstanceOf[A])
    case Schema.SLong => intItem(in).map(_.asInstanceOf[A])
    case Schema.SDouble => in.byte().flatMap {
      case 0xFB => in.long(8).map(java.lang.Double.longBitsToDouble(_).asInstanceOf[A])
      case b => Left(f"expected a double (0xFB), got 0x$b%02X")
    }
    case Schema.SBool => in.byte().flatMap {
      case 0xF4 => Right(false.asInstanceOf[A])
      case 0xF5 => Right(true.asInstanceOf[A])
      case b => Left(f"expected a boolean, got 0x$b%02X")
    }
    case Schema.SString => textItem(in).map(_.asInstanceOf[A])
    case Schema.SOption(of) =>
      if in.peek == 0xF6 then in.byte().map(_ => None.asInstanceOf[A])
      else get(in, of().asInstanceOf[Schema[Any]]).map(Some(_).asInstanceOf[A])
    case Schema.SList(of) =>
      head(in).flatMap {
        case (4, n) =>
          (0L until n).foldLeft(Right(List.empty[Any]): Either[String, List[Any]]) { (acc, _) =>
            acc.flatMap(xs => get(in, of().asInstanceOf[Schema[Any]]).map(xs :+ _))
          }.map(_.asInstanceOf[A])
        case (m, _) => Left(s"expected an array, got major $m")
      }
    case p: Schema.SProduct[A] =>
      head(in).flatMap {
        case (5, n) =>
          (0L until n).foldLeft(Right(Map.empty[String, Any]): Either[String, Map[String, Any]]) {
            (acc, _) =>
              acc.flatMap { m =>
                textItem(in).flatMap { k =>
                  p.fields.find(_._1 == k) match
                    case Some((_, sc)) => get(in, sc().asInstanceOf[Schema[Any]]).map(v => m + (k -> v))
                    case None => Left(s"unknown field '$k' of ${p.name}")
                }
              }
          }.flatMap { m =>
            p.fields.foldLeft(Right(Vector.empty[Any]): Either[String, Vector[Any]]) { (acc, f) =>
              acc.flatMap { xs =>
                (m.get(f._1), f._2()) match
                  case (None, _: Schema.SOption[?]) => Right(xs :+ None)
                  case (found, _) =>
                    found.toRight(s"missing field '${f._1}' in ${p.name}").map(xs :+ _)
              }
            }.map(p.make)
          }
        case (m, _) => Left(s"expected a map, got major $m")
      }
    case su: Schema.SSum[A] =>
      head(in).flatMap {
        case (5, 1) => textItem(in).flatMap { name =>
          su.cases.find(_._1 == name)
            .toRight(s"unknown case '$name' of ${su.name}")
            .flatMap((_, sc) => get(in, sc().asInstanceOf[Schema[Any]]).map(_.asInstanceOf[A]))
        }
        case (m, n) => Left(s"expected a one-entry map, got major $m of $n")
      }

  /** bytes to value in one move; errors as values */
  def read[A](bytes: Array[Byte])(using s: Schema[A]): Either[String, A] =
    get(In(bytes), s)
}
