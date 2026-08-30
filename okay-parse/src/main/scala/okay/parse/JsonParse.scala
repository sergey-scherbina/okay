package okay.parse

import okay.{Stage, pure}
import okay.lex.{Channel, Token}
import okay.lex.Json.K

/**
 * The proving dialect, both surfaces over the ONE instruction
 * language: a hand-written driver (bottom-up, the uniml style) and
 * the same grammar assembled from combinators (top-down). Both are
 * TOTAL — unexpected tokens become Bad instructions, structure
 * mismatches are the builder's problem (and it is total too) — and
 * lossless: trivia and punctuation are emitted, never skipped.
 */
object JsonParse {

  type T = Token[K]
  type D = Parse.Driver[K, Unit]

  private def tell(i: Instr[K]): Stage[T, Instr[K], Unit] =
    Stage.tell[T, Instr[K]](i).map(_ => ())

  /** the driver: structure opens and closes nodes, everything else is
   * emitted in place; the builder's totality absorbs mismatches */
  def driver: D =
    Stage.await[T, Instr[K]].flatMap {
      case None => pure(())
      case Some(t) => step(t).flatMap(_ => driver)
    }

  private def step(t: T): Stage[T, Instr[K], Unit] = t.kind match
    case K.LBrace => tell(Instr.Open("object", Some(t)))
    case K.LBracket => tell(Instr.Open("array", Some(t)))
    case K.RBrace | K.RBracket => tell(Instr.Close(Some(t)))
    case K.Bad => tell(Instr.Bad(Some(t), s"unexpected '${t.lexeme}'"))
    case _ => tell(Instr.Emit(t))

  // ------------------------------------------------------------------
  // the combinator surface: little total parsers over Take, compiling
  // to the same instructions

  object Comb {
    type P[A] = Stage[T, Instr[K], A]

    def next: P[Option[T]] = Stage.await[T, Instr[K]]

    def open(kind: String, t: T): P[Unit] = tell(Instr.Open(kind, Some(t)))
    def close(t: T): P[Unit] = tell(Instr.Close(Some(t)))
    def emit(t: T): P[Unit] = tell(Instr.Emit(t))
    def bad(t: T, why: String): P[Unit] = tell(Instr.Bad(Some(t), why))

    /** a value is whatever arrives, totally */
    def value: P[Unit] = next.flatMap {
      case None => pure(())
      case Some(t) => (t.kind match
        case K.LBrace => open("object", t)
        case K.LBracket => open("array", t)
        case K.RBrace | K.RBracket => close(t)
        case K.Bad => bad(t, s"unexpected '${t.lexeme}'")
        case _ => emit(t)
      ).flatMap(_ => value)
    }
  }

  /** the top-down surface: the same instructions, hence the same CST */
  def combinators: D = Comb.value
}
