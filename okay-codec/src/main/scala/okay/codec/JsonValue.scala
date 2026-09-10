package okay.codec

import Json.*
import okay.{Cont, reset, />}

/**
 * The fast VALUE parser beside the lossless one (specs/codecs.md,
 * "Value parser"). `Json.parse` is a lexer, a CST with every trivia
 * token, and a projection — the lossless layer, and 26x circe's
 * parser on a small object (staged-codecs step 0). Most callers want
 * the VALUE, so this is one strict recursive descent over the
 * string, no tokens, no tree: an index, a StringBuilder for the rare
 * escaped string, `parseDouble` on the number's slice.
 *
 * The contract that keeps it honest: it accepts ONLY what it is sure
 * of — RFC 8259's grammar, and the projection's own reading of it —
 * and answers None on anything else: a stray character, a trailing
 * value, a raw control character in a string, an unterminated
 * anything, empty input. `Json.parse` then hands such input to
 * the lossless parser, so damage gets exactly the CST's answer (the
 * JErr in place, the truncated tail, the message) and this parser
 * never has to reproduce a damage vocabulary. Agreement is a test
 * (TestJsonValue): every accepted document must equal what the
 * lossless road yields, and a prefix-truncation sweep checks the
 * refusals land on the lossless answer too.
 *
 * Two readings are the projection's, not RFC's, kept on purpose so
 * the two roads agree: `\\n` `\\t` `\\r` are the control characters,
 * `\\uXXXX` is the named UTF-16 code unit, and any OTHER escaped
 * character is itself (`\\b` is "b"); a number is whatever `toDouble`
 * makes of its RFC-shaped lexeme (`1e999` is Infinity).
 *
 * `\\uXXXX` was NOT decoded here until json-unicode-escape (2026-09-03):
 * both roads treated it as the literal characters "uXXXX", which this
 * comment used to document as the agreed reading rather than naming
 * as the bug it was. Found downstream, by a JSON producer (Telegram's
 * own Bot API, confirmed live) that escapes non-ASCII text — every
 * such character it sent came through as four to six garbage letters,
 * with nothing anywhere in the chain reporting an error.
 */
object JsonValue {

  /** the value, or None when this parser is not sure — never a throw */
  def parse(s: String): Option[Json] =
    val p = new Parser(s)
    p.skipWs()
    if p.at >= s.length then None
    else
      val v = p.value(0)
      if v == null then None
      else
        p.skipWs()
        if p.at == s.length then Some(v) else None

  /** null marks "not sure"; it never escapes this file */
  private final class Parser(s: String) {
    var at = 0
    private val n = s.length

    def skipWs(): Unit =
      while at < n && { val c = s.charAt(at); c == ' ' || c == '\n' || c == '\r' || c == '\t' } do at += 1

    /** `open` is how many containers are already open around this
     * value — counted for the native/trampoline switch, not a depth
     * REFUSAL (remove-codecs-maxdepth: there is no limit to refuse
     * past any more). PAST `Codecs.NativeThreshold`, dispatches to a
     * `Cont.defer` trampoline (json-raw-nesting-threshold-trampoline)
     * — the same design as the three already-closed sites, simpler
     * than the two schema-decoding ones: uniformly typed
     * (`Json | Null` throughout, like `Cbor.In.skipItem`'s
     * `Either[String, Unit]`), so no cross-type `R` to thread. */
    def value(open: Int): Json | Null =
      if open >= Codecs.NativeThreshold then reset(valueC[Json | Null](open))
      else valueNative(open)

    private def valueNative(open: Int): Json | Null =
      if at >= n then null
      else s.charAt(at) match
        case '{' => objNative(open + 1)
        case '[' => arrNative(open + 1)
        case '"' => str() match { case null => null; case x => JStr(x) }
        case 't' => lit("true", JBool(true))
        case 'f' => lit("false", JBool(false))
        case 'n' => lit("null", JNull)
        case c if c == '-' || (c >= '0' && c <= '9') => num()
        case _ => null

    private def lit(word: String, v: Json): Json | Null =
      if s.startsWith(word, at) then { at += word.length; v } else null

    private def objNative(open: Int): Json | Null =
      at += 1
      val b = Vector.newBuilder[(String, Json)]
      skipWs()
      if at < n && s.charAt(at) == '}' then { at += 1; JObj(Vector.empty) }
      else
        var ok = true
        var done = false
        while ok && !done do
          skipWs()
          if at >= n || s.charAt(at) != '"' then ok = false
          else
            val k = str()
            if k == null then ok = false
            else
              skipWs()
              if at >= n || s.charAt(at) != ':' then ok = false
              else
                at += 1
                skipWs()
                val v = value(open)          // the DISPATCHER: depth
                                              // may cross the threshold
                                              // mid-object
                if v == null then ok = false
                else
                  b += ((k, v))
                  skipWs()
                  if at >= n then ok = false
                  else s.charAt(at) match
                    case ',' => at += 1
                    case '}' => at += 1; done = true
                    case _ => ok = false
        if ok then JObj(b.result()) else null

    private def arrNative(open: Int): Json | Null =
      at += 1
      val b = Vector.newBuilder[Json]
      skipWs()
      if at < n && s.charAt(at) == ']' then { at += 1; JArr(Vector.empty) }
      else
        var ok = true
        var done = false
        while ok && !done do
          skipWs()
          val v = value(open)              // the DISPATCHER
          if v == null then ok = false
          else
            b += v
            skipWs()
            if at >= n then ok = false
            else s.charAt(at) match
              case ',' => at += 1
              case ']' => at += 1; done = true
              case _ => ok = false
        if ok then JArr(b.result()) else null

    // ---- the trampoline: mirrors valueNative/objNative/arrNative
    // exactly, deferring the ONE point each descends into a fresh
    // value through Cont.defer, so the reader's mutable `at` cursor
    // still advances in the same order, just inside `/`'s loop
    // instead of the native call stack ----

    private def valueC[R](open: Int): (Json | Null) /> R =
      if at >= n then Cont.Pure(null)
      else s.charAt(at) match
        case '{' => objC[R](open + 1)
        case '[' => arrC[R](open + 1)
        case '"' => Cont.Pure(str() match { case null => null; case x => JStr(x) })
        case 't' => Cont.Pure(lit("true", JBool(true)))
        case 'f' => Cont.Pure(lit("false", JBool(false)))
        case 'n' => Cont.Pure(lit("null", JNull))
        case c if c == '-' || (c >= '0' && c <= '9') => Cont.Pure(num())
        case _ => Cont.Pure(null)

    private def objC[R](open: Int): (Json | Null) /> R =
      at += 1
      val b = Vector.newBuilder[(String, Json)]
      skipWs()
      if at < n && s.charAt(at) == '}' then { at += 1; Cont.Pure(JObj(Vector.empty)) }
      else
        def loop(): (Json | Null) /> R =
          skipWs()
          if at >= n || s.charAt(at) != '"' then Cont.Pure(null)
          else
            val k = str()
            if k == null then Cont.Pure(null)
            else
              skipWs()
              if at >= n || s.charAt(at) != ':' then Cont.Pure(null)
              else
                at += 1
                skipWs()
                Cont.defer(() => valueC[R](open)) { v =>
                  if v == null then Cont.Pure(null)
                  else
                    b += ((k, v))
                    skipWs()
                    if at >= n then Cont.Pure(null)
                    else s.charAt(at) match
                      case ',' => at += 1; loop()
                      case '}' => at += 1; Cont.Pure(JObj(b.result()))
                      case _ => Cont.Pure(null)
                }
        loop()

    private def arrC[R](open: Int): (Json | Null) /> R =
      at += 1
      val b = Vector.newBuilder[Json]
      skipWs()
      if at < n && s.charAt(at) == ']' then { at += 1; Cont.Pure(JArr(Vector.empty)) }
      else
        def loop(): (Json | Null) /> R =
          skipWs()
          Cont.defer(() => valueC[R](open)) { v =>
            if v == null then Cont.Pure(null)
            else
              b += v
              skipWs()
              if at >= n then Cont.Pure(null)
              else s.charAt(at) match
                case ',' => at += 1; loop()
                case ']' => at += 1; Cont.Pure(JArr(b.result()))
                case _ => Cont.Pure(null)
          }
        loop()

    /** the string's content; the fast road is the slice when no
     * escape appears, the builder otherwise */
    private def str(): String | Null =
      at += 1
      val start = at
      var i = at
      var plain = true
      var closed = false
      while !closed && i < n do
        val c = s.charAt(i)
        if c == '"' then closed = true
        else if c == '\\' then { plain = false; i += 2 }
        else if c < ' ' then { i = n }   // a raw control character: not ours
        else i += 1
      if !closed || i > n then null
      else if plain then { at = i + 1; s.substring(start, i) }
      else
        // the projection's unquote, verbatim: n t r are control
        // characters, \uXXXX is one UTF-16 code unit (a surrogate
        // pair needs nothing extra — two appended code units that
        // form one are already a correct String), and any OTHER
        // escaped character is itself
        val b = new java.lang.StringBuilder(i - start)
        var j = start
        while j < i do
          val c = s.charAt(j)
          if c == '\\' && j + 1 < i then
            s.charAt(j + 1) match
              case 'n' => b.append('\n'): Unit; j += 2
              case 't' => b.append('\t'): Unit; j += 2
              case 'r' => b.append('\r'): Unit; j += 2
              case 'u' if j + 6 <= i =>
                Json.hex4(s.substring(j + 2, j + 6)) match
                  case Some(ch) => b.append(ch): Unit; j += 6
                  case None => b.append('u'): Unit; j += 2
              case x => b.append(x): Unit; j += 2
          else { b.append(c): Unit; j += 1 }
        at = i + 1
        b.toString

    /** RFC 8259 number: -? int frac? exp?; the value is toDouble's */
    private def num(): Json | Null =
      val start = at
      var i = at
      if s.charAt(i) == '-' then i += 1
      if i >= n then return null
      if s.charAt(i) == '0' then i += 1
      else if s.charAt(i) >= '1' && s.charAt(i) <= '9' then
        while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' do i += 1
      else return null
      if i < n && s.charAt(i) == '.' then
        i += 1
        val fs = i
        while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' do i += 1
        if i == fs then return null
      if i < n && (s.charAt(i) == 'e' || s.charAt(i) == 'E') then
        i += 1
        if i < n && (s.charAt(i) == '+' || s.charAt(i) == '-') then i += 1
        val es = i
        while i < n && s.charAt(i) >= '0' && s.charAt(i) <= '9' do i += 1
        if i == es then return null
      at = i
      JNum(java.lang.Double.parseDouble(s.substring(start, i)))
  }
}
