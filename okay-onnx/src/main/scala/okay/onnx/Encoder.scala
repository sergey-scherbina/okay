package okay.onnx

import ai.djl.huggingface.tokenizers.HuggingFaceTokenizer
import ai.onnxruntime.{OnnxJavaType, OnnxTensor, OrtEnvironment, OrtSession, TensorInfo}
import okay.Handler
import okay.rag.{Embed, Embedding, Token, Tokens, Vectors, embedding}
import java.nio.file.Path
import scala.jdk.CollectionConverters.*

/**
 * The direct session (specs/intent-spans.md): a sentence encoder
 * opened by our own hands, so that what it computes for every token
 * comes back beside what it pools them into.
 *
 * `okay-langchain4j-embed` wraps the same file and answers one vector
 * per text — mean over every position, then unit length. This answers
 * that vector, computed the same way from the same forward pass (a
 * consumer measured cosine 1.0000 between the two), AND the token
 * vectors with their characters, which the wrapper has no door for.
 * One pass, two readings; not a second encoder.
 *
 * A directory with `model.onnx` and `tokenizer.json`, as the
 * sentence-transformers exports ship. Thread-safe the way the runtime
 * is: one session, `run` from any thread.
 */
final class Encoder(modelDir: Path, mostTokens: Int = Encoder.mostTokens) extends AutoCloseable:

  private val tokenizer = HuggingFaceTokenizer.newInstance(modelDir.resolve("tokenizer.json"),
    Map("truncation" -> "true", "maxLength" -> mostTokens.toString).asJava)
  private val env = OrtEnvironment.getEnvironment
  private val session = env.createSession(modelDir.resolve("model.onnx").toString,
    new OrtSession.SessionOptions)

  /** the model's first output, checked ONCE to be what `hidden` reads:
   * `float32[batch, sequence, dim]`. A model of another shape fails
   * here, at open, and not per message */
  private val output: String =
    val name = session.getOutputNames.iterator.next
    session.getOutputInfo.get(name).getInfo match
      case t: TensorInfo if t.`type` == OnnxJavaType.FLOAT && t.getShape.length == 3 => name
      case other => throw IllegalArgumentException(
        s"$modelDir: output `$name` is $other, not float[batch, sequence, dim]")

  /** the number of dimensions of one vector */
  val dim: Int =
    session.getOutputInfo.get(output).getInfo match
      case t: TensorInfo => t.getShape.last.toInt
      case _ => 0

  /** one forward pass, read two ways */
  final case class Encoded(tokens: Vector[Token], pooled: Embedding)

  def encode(text: String): Encoded =
    val enc = tokenizer.encode(text)
    val in = Map(
      "input_ids" -> OnnxTensor.createTensor(env, Array(enc.getIds)),
      "attention_mask" -> OnnxTensor.createTensor(env, Array(enc.getAttentionMask)),
      "token_type_ids" -> OnnxTensor.createTensor(env, Array(enc.getTypeIds))).asJava
    val out = session.run(in)
    try
      val rows = hidden(out)
      val spans = enc.getCharTokenSpans
      val words = enc.getTokens
      val tokens = rows.indices.toVector.flatMap { i =>
        val s = spans(i)
        // a special token has no characters, and that is how it is told
        // from a word — not by its spelling, which is the tokenizer's
        if s == null || Encoder.specials(words(i)) then None
        else Some(Token(words(i), s.getStart, s.getEnd, embedding(rows(i))))
      }
      Encoded(tokens, Encoder.pool(rows))
    finally
      out.close()
      in.values.forEach(_.close())

  /**
   * THE ONE CAST. The runtime hands back `Object`; the model's own
   * signature says the first output is `float32[batch, sequence, dim]`,
   * which in Java is `float[][][]` — checked at open (`output`), so
   * the type here is the one the model declared and not a hope.
   */
  private def hidden(r: OrtSession.Result): Array[Array[Float]] =
    r.get(0).getValue.asInstanceOf[Array[Array[Array[Float]]]](0)

  /** the pooled vector alone — the production embedding, as a plain
   * function, the shape a store's constructor takes */
  def embed: String => Embedding = text => encode(text).pooled

  /** the token vectors alone — the seam `okay.intent.Spans` reads */
  def tokens: Tokens = text => encode(text).tokens

  /** okay-rag's effect, one call per text as `Langchain4jEmbed.handler` */
  def handler: Handler[Embed] = new:
    def handle[A](e: Embed[A]): A = e match
      case Embed.Of(texts) => texts.map(embed)

  def close(): Unit =
    session.close()
    tokenizer.close()

object Encoder:
  /** the two specials the sentence-transformers tokenizers add; a
   * token with no characters is dropped regardless, this is only the
   * name check beside it */
  val specials: Set[String] = Set("<s>", "</s>", "[CLS]", "[SEP]")

  /** the encoder's own window; a longer text is truncated, said here
   * rather than discovered as a runtime error. The wrapper this
   * replaces partitions and averages instead; a chat message is a
   * sentence, and a document is not this module's business */
  val mostTokens: Int = 512

  /** mean over every position, then unit length — what the wrapper
   * computes, so a consumer that switches sees the vectors it had */
  def pool(rows: Array[Array[Float]]): Embedding =
    val dim = rows.headOption.map(_.length).getOrElse(0)
    val acc = Array.fill(dim)(0.0f)
    for r <- rows do
      var i = 0
      while i < dim do
        acc(i) += r(i)
        i += 1
    Vectors.normalize(embedding(acc.map(_ / rows.length)))
