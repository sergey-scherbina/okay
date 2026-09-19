package okay

import java.nio.charset.StandardCharsets.UTF_8

/**
 * UTF-8 lines out of a byte stream — the framer every line protocol
 * shares.
 *
 * Written for okay-http and moved here when okay-cluster turned out to
 * need it without the module: it has no dependency, and a
 * `Stage[Chunk[Byte], String, Unit]` is core-stream vocabulary
 * (`Stage.transduce`, like `llm.Sse.events`).
 *
 * Framing happens on BYTES, before decoding, and that is not
 * fussiness: a chunk boundary can fall inside a multi-byte UTF-8
 * sequence, and decoding each chunk separately would corrupt it. A
 * newline byte cannot appear inside such a sequence (continuation
 * bytes are all >= 0x80), so splitting bytes first and decoding whole
 * lines after is both simpler and correct — and the test that pins it
 * splits a multi-byte character in half.
 */
object Lines {

  /** bytes in, lines out; CRLF stripped; a trailing line without a
   * newline is still a line */
  def stage: Stage[Chunk[Byte], String, Unit] =
    def spill(buf: Array[Byte]): Stage[Chunk[Byte], String, Array[Byte]] =
      var i = 0
      while i < buf.length && buf(i) != '\n'.toByte do i += 1
      if i >= buf.length then pure(buf)
      else
        val line = new String(buf, 0, if i > 0 && buf(i - 1) == '\r' then i - 1 else i, UTF_8)
        Stage.tell[Chunk[Byte], String](line)
          .flatMap(_ => spill(java.util.Arrays.copyOfRange(buf, i + 1, buf.length)))

    val framed: Stage[Chunk[Byte], String, Array[Byte]] =
      Stage.transduce(Array.empty[Byte])(
        (buf, c) => spill(buf ++ c.toArray),
        rest =>
          if rest.isEmpty then pure(rest)
          else Stage.tell[Chunk[Byte], String](new String(rest, UTF_8))
            .map(_ => Array.empty[Byte]))

    framed.map(_ => ())
}
