package okay2.codec

import Json._
import okay2.{Cont, reset, />}

/**
 * The fast VALUE parser beside the lossless one (okay-codec's
 * JsonValue.scala): one strict recursive descent over the string — an
 * index, a slice for a plain string, `parseDouble` on a number's slice,
 * no tokens and no tree.
 *
 * It accepts ONLY what it is sure of — RFC 8259's grammar, read the
 * way the projection reads it — and answers None on anything else,
 * which `Json.parse` hands to the lossless parser; so damage gets
 * exactly the CST's answer and this parser keeps no damage vocabulary.
 * Agreement is a test (TestJsonValue): an accepted document equals the
 * lossless road's value, and a truncation sweep lands every refusal on
 * the lossless answer.
 *
 * The readings are the projection's: `\n` `\t` `\r` are the control
 * characters, `\uXXXX` one UTF-16 code unit, any OTHER escaped
 * character is itself; a number is whatever `parseDouble` makes of its
 * RFC-shaped lexeme.
 */
object JsonValue {

  /** the value, or None when this parser is not sure — never a throw */
  def parse(s: String): Option[Json] = {
    val p = new Parser(s)
    p.skipWs()
    if (p.at >= s.length) None
    else {
      val v = p.value(0)
      if (v == null) None
      else {
        p.skipWs()
        if (p.at == s.length) Some(v) else None
      }
    }
  }

  /** null marks "not sure"; it never escapes this file */
  private final class Parser(s: String) {
    var at = 0
    private val n = s.length

    def skipWs(): Unit =
      while (at < n && { val c = s.charAt(at); c == ' ' || c == '\n' || c == '\r' || c == '\t' }) at += 1

    /** `open` counts the containers already open around this value:
     * past `Codecs.NativeThreshold` the descent continues on the
     * trampoline, so depth is bounded by the heap, not the stack */
    def value(open: Int): Json =
      if (open >= Codecs.NativeThreshold) reset(valueC[Json](open))
      else valueNative(open)

    private def scalar(): Json = s.charAt(at) match {
      case '"' => val x = str(); if (x == null) null else JStr(x)
      case 't' => lit("true", JBool(true))
      case 'f' => lit("false", JBool(false))
      case 'n' => lit("null", JNull)
      case c if c == '-' || (c >= '0' && c <= '9') => num()
      case _ => null
    }

    private def valueNative(open: Int): Json =
      if (at >= n) null
      else s.charAt(at) match {
        case '{' => objNative(open + 1)
        case '[' => arrNative(open + 1)
        case _ => scalar()
      }

    private def lit(word: String, v: Json): Json =
      if (s.startsWith(word, at)) { at += word.length; v } else null

    /** after a key: the colon, and the whitespace around it; false when
     * the object is not well-formed here */
    private def colon(): Boolean = {
      skipWs()
      if (at >= n || s.charAt(at) != ':') false
      else { at += 1; skipWs(); true }
    }

    private def objNative(open: Int): Json = {
      at += 1
      val b = Vector.newBuilder[(String, Json)]
      skipWs()
      if (at < n && s.charAt(at) == '}') { at += 1; JObj(Vector.empty) }
      else {
        var ok = true
        var done = false
        while (ok && !done) {
          skipWs()
          if (at >= n || s.charAt(at) != '"') ok = false
          else {
            val k = str()
            if (k == null || !colon()) ok = false
            else {
              val v = value(open)
              if (v == null) ok = false
              else {
                b += ((k, v))
                skipWs()
                if (at >= n) ok = false
                else s.charAt(at) match {
                  case ',' => at += 1
                  case '}' => at += 1; done = true
                  case _ => ok = false
                }
              }
            }
          }
        }
        if (ok) JObj(b.result()) else null
      }
    }

    private def arrNative(open: Int): Json = {
      at += 1
      val b = Vector.newBuilder[Json]
      skipWs()
      if (at < n && s.charAt(at) == ']') { at += 1; JArr(Vector.empty) }
      else {
        var ok = true
        var done = false
        while (ok && !done) {
          skipWs()
          val v = value(open)
          if (v == null) ok = false
          else {
            b += v
            skipWs()
            if (at >= n) ok = false
            else s.charAt(at) match {
              case ',' => at += 1
              case ']' => at += 1; done = true
              case _ => ok = false
            }
          }
        }
        if (ok) JArr(b.result()) else null
      }
    }

    // ---- the trampoline: the native descent case for case, each
    // descent into a fresh value a Cont.defer, so the mutable `at`
    // still advances in the same order, inside reset's loop ----

    private def valueC[R](open: Int): Json /> R =
      if (at >= n) Cont.Pure(null)
      else s.charAt(at) match {
        case '{' => objC[R](open + 1)
        case '[' => arrC[R](open + 1)
        case _ => Cont.Pure(scalar())
      }

    private def objC[R](open: Int): Json /> R = {
      at += 1
      val b = Vector.newBuilder[(String, Json)]
      skipWs()
      if (at < n && s.charAt(at) == '}') { at += 1; Cont.Pure(JObj(Vector.empty)) }
      else {
        def loop(): Json /> R = {
          skipWs()
          if (at >= n || s.charAt(at) != '"') Cont.Pure(null)
          else {
            val k = str()
            if (k == null || !colon()) Cont.Pure(null)
            else Cont.defer(() => valueC[R](open)) { (v: Json) =>
              if (v == null) Cont.Pure[Json, R](null)
              else {
                b += ((k, v))
                skipWs()
                if (at >= n) Cont.Pure[Json, R](null)
                else s.charAt(at) match {
                  case ',' => at += 1; loop()
                  case '}' => at += 1; Cont.Pure[Json, R](JObj(b.result()))
                  case _ => Cont.Pure[Json, R](null)
                }
              }
            }
          }
        }
        loop()
      }
    }

    private def arrC[R](open: Int): Json /> R = {
      at += 1
      val b = Vector.newBuilder[Json]
      skipWs()
      if (at < n && s.charAt(at) == ']') { at += 1; Cont.Pure(JArr(Vector.empty)) }
      else {
        def loop(): Json /> R = {
          skipWs()
          Cont.defer(() => valueC[R](open)) { (v: Json) =>
            if (v == null) Cont.Pure[Json, R](null)
            else {
              b += v
              skipWs()
              if (at >= n) Cont.Pure[Json, R](null)
              else s.charAt(at) match {
                case ',' => at += 1; loop()
                case ']' => at += 1; Cont.Pure[Json, R](JArr(b.result()))
                case _ => Cont.Pure[Json, R](null)
              }
            }
          }
        }
        loop()
      }
    }

    /** the string's content: the slice when no escape appears, the
     * builder otherwise; null when unterminated or holding a raw
     * control character */
    private def str(): String = {
      at += 1
      val start = at
      var i = at
      var plain = true
      var closed = false
      while (!closed && i < n) {
        val c = s.charAt(i)
        if (c == '"') closed = true
        else if (c == '\\') { plain = false; i += 2 }
        else if (c < ' ') i = n
        else i += 1
      }
      if (!closed || i > n) null
      else if (plain) { at = i + 1; s.substring(start, i) }
      else {
        val b = new java.lang.StringBuilder(i - start)
        var j = start
        while (j < i) {
          val c = s.charAt(j)
          if (c == '\\' && j + 1 < i) {
            s.charAt(j + 1) match {
              case 'n' => b.append('\n'); j += 2
              case 't' => b.append('\t'); j += 2
              case 'r' => b.append('\r'); j += 2
              case 'u' if j + 6 <= i =>
                Json.hex4(s.substring(j + 2, j + 6)) match {
                  case Some(ch) => b.append(ch); j += 6
                  case None => b.append('u'); j += 2
                }
              case x => b.append(x); j += 2
            }
          } else { b.append(c); j += 1 }
        }
        at = i + 1
        b.toString
      }
    }

    /** RFC 8259 number: -? int frac? exp?; the value is parseDouble's */
    private def num(): Json = {
      val start = at
      var i = at
      if (s.charAt(i) == '-') i += 1
      def digits(): Unit = while (i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
      val intOk =
        if (i >= n) false
        else if (s.charAt(i) == '0') { i += 1; true }
        else if (s.charAt(i) >= '1' && s.charAt(i) <= '9') { digits(); true }
        else false
      val fracOk = !intOk || !(i < n && s.charAt(i) == '.') || {
        i += 1
        val fs = i
        digits()
        i > fs
      }
      val expOk = !intOk || !fracOk || !(i < n && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) || {
        i += 1
        if (i < n && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
        val es = i
        digits()
        i > es
      }
      if (!(intOk && fracOk && expOk)) null
      else {
        at = i
        JNum(java.lang.Double.parseDouble(s.substring(start, i)))
      }
    }
  }
}
