package okay.llm

import okay.{!, %, +, Async, Writer}
import okay.given
import okay.codec.{Json, Schema}
import okay.lex.Json as JsonLex
import okay.parse.{Cst, JsonParse, Parse}

/**
 * Structured output, validated AS IT ARRIVES — and generation cut the
 * moment the value is complete.
 *
 * This is what the total, incremental parser was for. Every arriving
 * token extends the text by an append, which is an EDIT, so
 * `Parse.reparse` re-drives only the damage instead of re-parsing the
 * whole buffer: the per-token cost is the token, not the answer so
 * far. After each one the tree is checked — no holes and a successful
 * decode means the value is structurally complete — and the stream is
 * simply not pulled again. Since the stream is demand-driven, not
 * pulling IS cancelling: the tokens after the closing brace are never
 * generated, and never paid for.
 *
 * Honest scope: the stream is assumed to BE the JSON (a model told to
 * answer with a value and nothing else). Prose around it makes the
 * tree error-bearing, so completion is never declared and the walk
 * runs to the end — the safe direction.
 */
object Structured {

  /** what a walk found, and what it cost */
  final case class Cut[A](value: Option[A], text: String, tokens: Int, stopped: Boolean)

  /**
   * Pull the token stream until the accumulated text decodes as `A`,
   * then stop. Returns the value, the text consumed, how many tokens
   * that took, and whether the walk stopped EARLY (the interesting
   * bit: false means the stream ended first).
   */
  def cut[A](tokens: Unit ! (Writer % String + Async))
            (using s: Schema[A], h: okay.Handler[Async]): Cut[A] =
    import okay.!.*
    type F = Writer % String + Async

    var text = ""
    var session = Parse.full(JsonLex.scan, JsonParse.instrs)("")
    var count = 0

    /** extend by one token — an append is an edit, so this is O(token) */
    def feed(tok: String): Option[A] =
      val next = text + tok
      session = Parse.reparse(JsonLex.scan, JsonParse.instrs)(
        session, text, next, text.length, text.length, next.length)
      text = next
      count += 1
      // a tree with no holes AND a value that fits the schema
      if Cst.errors(session.tree).nonEmpty then None
      else Json.decode(s)(Json.value(session.tree)).toOption

    def walk(rest: Unit ! F): Cut[A] = (rest.resume: @unchecked) match
      case Pure(_) => Cut(None, text, count, stopped = false)
      case Effect(e) => okay.<|>[Async, Writer % String](e) match
        case Left(a) => h.handle(a); Cut(None, text, count, stopped = false)
        case Right(Writer.Say(w)) => feed(w) match
          case Some(v) => Cut(Some(v), text, count, stopped = true)
          case None => Cut(None, text, count, stopped = false)
      case Bind(Effect(e), k) => okay.<|>[Async, Writer % String](e) match
        case Left(a) => walk(k(h.handle(a)))
        case Right(Writer.Say(w)) => feed(w) match
          // complete: the rest of the stream is never pulled, which
          // for a live model means it is never generated
          case Some(v) => Cut(Some(v), text, count, stopped = true)
          case None => walk(k(()))

    walk(tokens)

  /** the value alone, for callers who do not care what it cost */
  def first[A](tokens: Unit ! (Writer % String + Async))
              (using Schema[A], okay.Handler[Async]): Option[A] =
    cut[A](tokens).value
}
