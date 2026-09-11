package okay.rag

/**
 * A vector with a LOCATION (specs/intent-spans.md).
 *
 * `Embedding` is what a sentence encoder answers for a text; this is
 * what it computed for each token on the way there, kept. `start`
 * and `end` are character offsets into the text the token came from,
 * half-open, so a window of words can ask which tokens overlap it
 * without knowing how the tokenizer split them — «Вроцлаве» is three
 * pieces to the tokenizer and one word to a person.
 *
 * Not `okay.lex.Span`, deliberately: that is a lexer's position with
 * a line and a column, for a document; this is two offsets into one
 * sentence, and a line number would be a lie.
 */
final case class Token(text: String, start: Int, end: Int, vector: Embedding)

/** an encoder that keeps its tokens — the seam `okay.intent.Spans`
 * reads through, and the one `okay.onnx.Encoder` implements */
type Tokens = String => Vector[Token]
