package okay.codec

import Json.*

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
 * anything, empty input. `Json.parseValue` then hands such input to
 * the lossless parser, so damage gets exactly the CST's answer (the
 * JErr in place, the truncated tail, the message) and this parser
 * never has to reproduce a damage vocabulary. Agreement is a test
 * (TestJsonValue): every accepted document must equal what the
 * lossless road yields, and a prefix-truncation sweep checks the
 * refusals land on the lossless answer too.
 *
 * Two readings are the projection's, not RFC's, kept on purpose so
 * the two roads agree: escapes `\\n` `\\t` `\\r` are the control
 * characters and ANY other escaped character is itself (`\\u0041` is
 * the four letters "u0041", `\\b` is "b"); a number is whatever
 * `toDouble` makes of its RFC-shaped lexeme (`1e999` is Infinity).
 */
object JsonValue {

  /** the value, or None when this parser is not sure — never a throw */
  def parse(s: String): Option[Json] =
    val p = new Parser(s)
    p.skipWs()
    if p.at >= s.length then None
    else
      val v = p.value()
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

    def value(): Json | Null =
      if at >= n then null
      else s.charAt(at) match
        case '{' => obj()
        case '[' => arr()
        case '"' => str() match { case null => null; case x => JStr(x) }
        case 't' => lit("true", JBool(true))
        case 'f' => lit("false", JBool(false))
        case 'n' => lit("null", JNull)
        case c if c == '-' || (c >= '0' && c <= '9') => num()
        case _ => null

    private def lit(word: String, v: Json): Json | Null =
      if s.startsWith(word, at) then { at += word.length; v } else null

    private def obj(): Json | Null =
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
                val v = value()
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

    private def arr(): Json | Null =
      at += 1
      val b = Vector.newBuilder[Json]
      skipWs()
      if at < n && s.charAt(at) == ']' then { at += 1; JArr(Vector.empty) }
      else
        var ok = true
        var done = false
        while ok && !done do
          skipWs()
          val v = value()
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
        // characters, any other escaped character is itself
        val b = new java.lang.StringBuilder(i - start)
        var j = start
        while j < i do
          val c = s.charAt(j)
          if c == '\\' && j + 1 < i then
            s.charAt(j + 1) match
              case 'n' => b.append('\n'): Unit
              case 't' => b.append('\t'): Unit
              case 'r' => b.append('\r'): Unit
              case x => b.append(x): Unit
            j += 2
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
