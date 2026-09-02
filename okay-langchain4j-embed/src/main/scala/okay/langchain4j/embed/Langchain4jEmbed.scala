package okay.langchain4j.embed

import okay.Handler
import okay.rag.{Embed, Embedding, embedding}
import dev.langchain4j.model.embedding.EmbeddingModel

/**
 * The interop sentence's EmbeddingModel half (specs/llm-agentic.md,
 * rag-langchain4j): their `EmbeddingModel` as our `Embedding` seam,
 * two ways — a plain function (the shape `MemoryMatch`'s constructor
 * already accepts, no `okay-match` change needed) and `okay-rag`'s
 * `Embed` effect (for a program built against `Retrieve.vector`).
 *
 * A separate module from `okay-langchain4j` (the chat/Model half):
 * this one pulls in a real local model artifact, kept out of the
 * root build for that reason (build.sbt).
 */
object Langchain4jEmbed {

  /** their model as a plain function */
  def embed(model: EmbeddingModel): String => Embedding = text =>
    embedding(model.embed(text).content().vector())

  /** their model as okay-rag's effect handler — one call per text,
   * the same as their own API (no batch endpoint to prefer) */
  def handler(model: EmbeddingModel): Handler[Embed] = new:
    private val f = embed(model)
    def handle[A](e: Embed[A]): A = e match
      case Embed.Of(texts) => texts.map(f)
}
