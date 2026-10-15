package okay.llm

import okay.{!, %, +, Async, Handler, Walk}
import okay.std.{Writer}
import okay.codec.{Json, Schema}
import okay.lex.Json as JsonLex
import okay.parse.{Cst, JsonParse, Parse}
import scala.annotation.tailrec

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
  def cut[A](tokens: Unit ! Writer % String + Async)
            (using s: Schema[A], h: okay.Answers[Async]): Cut[A] =

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
      else okay.codec.Codecs.json(s).decode(Json.value(session.tree)).toOption

    // the Async operations answered in place by `h` as the walk pulls; the tells walked one by one
    val told: Unit ! Writer % String =
      tokens.handle(Handler.answering[Async]([X] => (op: Async[X]) => h.handle(op)))

    @tailrec def walk(rest: Unit ! Writer % String): Cut[A] = Walk.view(rest) match
      case Walk.Done(_) => Cut(None, text, count, stopped = false)
      case o: Walk.Op[Writer % String, x, Unit, Writer % String] => said(o) match
        // complete: the rest of the stream is never pulled, which
        // for a live model means it is never generated
        case Left(v) => Cut(Some(v), text, count, stopped = true)
        case Right(next) => walk(next)

    // the constructor gives the told token AND the answer type Unit — no cast on either half
    def said[X](o: Walk.Op[Writer % String, X, Unit, Writer % String]): Either[A, Unit ! Writer % String] =
      (o.op: Writer[String, X]) match
        case Writer.Say(w) => feed(w).toLeft(o.resume(()))

    walk(told)

  /** the value alone, for callers who do not care what it cost */
  def first[A](tokens: Unit ! Writer % String + Async)
              (using Schema[A], okay.Answers[Async]): Option[A] =
    cut[A](tokens).value
}
