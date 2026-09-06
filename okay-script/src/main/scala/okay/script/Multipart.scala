package okay.script

import okay.script.api.Part

import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}

/** A byte-level `multipart/form-data` parser -- binary-safe, no
 * dependency. See specs/okay-script.md "Uploads".
 *
 * Framing per RFC 7578/2046: parts are delimited by `CRLF--boundary`,
 * the body may start with `--boundary` directly (no preceding CRLF),
 * each part's headers end at the first empty line, and `--boundary--`
 * closes. Anything before the first delimiter (a preamble) or after
 * the close (an epilogue) is ignored. A body that does not contain
 * the boundary yields no parts -- damage is a missing field, never an
 * exception.
 */
object Multipart:

  /** the `boundary` parameter of a Content-Type, quoted or bare */
  def boundaryOf(contentType: String): Option[String] =
    contentType.split(";").iterator.map(_.trim).collectFirst {
      case p if p.regionMatches(true, 0, "boundary=", 0, 9) =>
        val v = p.substring(9).trim
        if v.length >= 2 && v.startsWith("\"") && v.endsWith("\"") then v.substring(1, v.length - 1) else v
    }.filter(_.nonEmpty)

  def parse(body: Array[Byte], boundary: String): Vector[Part] =
    val delim = ("--" + boundary).getBytes(ISO_8859_1)
    val crlf = "\r\n".getBytes(ISO_8859_1)
    val out = Vector.newBuilder[Part]
    // the first delimiter may sit at offset 0 (no leading CRLF)
    var pos = indexOf(body, delim, 0)
    while pos >= 0 do
      var p = pos + delim.length
      // closing delimiter?
      if p + 1 < body.length && body(p) == '-' && body(p + 1) == '-' then pos = -1
      else
        // skip the CRLF (or transport padding) after the delimiter
        val eol = indexOf(body, crlf, p)
        if eol < 0 then pos = -1
        else
          p = eol + crlf.length
          // headers: up to the first empty line
          val headers = scala.collection.mutable.ArrayBuffer.empty[(String, String)]
          var more = true
          while more do
            val e = indexOf(body, crlf, p)
            if e < 0 then
              more = false
              pos = -1
            else
              val line = new String(body, p, e - p, UTF_8)
              p = e + crlf.length
              if line.isEmpty then more = false
              else
                val i = line.indexOf(':')
                if i > 0 then headers += (line.substring(0, i).trim -> line.substring(i + 1).trim)
          if pos >= 0 then
            // content: up to CRLF + next delimiter
            val next = indexOf(body, crlf ++ delim, p)
            val end = if next < 0 then body.length else next
            val bytes = java.util.Arrays.copyOfRange(body, p, end)
            val disposition = headers.collectFirst { case (k, v) if k.equalsIgnoreCase("content-disposition") => v }.getOrElse("")
            val name = param(disposition, "name").getOrElse("")
            val filename = param(disposition, "filename")
            val ct = headers.collectFirst { case (k, v) if k.equalsIgnoreCase("content-type") => v }
            out += Part(name, filename, ct, bytes)
            pos = if next < 0 then -1 else next + crlf.length
    out.result()

  /** `name="v"` or `name=v` inside a header value's parameters */
  private def param(header: String, name: String): Option[String] =
    header.split(";").iterator.map(_.trim).collectFirst {
      case p if p.regionMatches(true, 0, name + "=", 0, name.length + 1) =>
        val v = p.substring(name.length + 1).trim
        if v.length >= 2 && v.startsWith("\"") && v.endsWith("\"") then v.substring(1, v.length - 1) else v
    }

  private def indexOf(hay: Array[Byte], needle: Array[Byte], from: Int): Int =
    if needle.isEmpty then from
    else
      var i = math.max(from, 0)
      val last = hay.length - needle.length
      var found = -1
      while found < 0 && i <= last do
        if hay(i) == needle(0) then
          var j = 1
          while j < needle.length && hay(i + j) == needle(j) do j += 1
          if j == needle.length then found = i
        i += 1
      found
