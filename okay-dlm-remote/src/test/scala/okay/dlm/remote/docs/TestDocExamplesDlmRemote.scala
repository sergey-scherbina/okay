package okay.dlm.remote.docs

import munit.FunSuite

/**
 * The two blocks that name okay-dlm-remote, every line held verbatim
 * (dlm-doc-pinned, 2026-09-28; doc-snippets-pin-all's rule):
 *
 *  - docs/modules/okay-dlm-remote.md: Laya as the judge with ours behind
 *    it, a head built over it — RUN: building a head sends nothing, so
 *    it is checkable offline.
 *  - docs/modules/okay-dlm.md, "Backends", the composition root —
 *    COMPILED, not run: it names a model on disk (`onnx.embed`, a
 *    stand-in below with the ONNX encoder's shape) and a paid judge whose
 *    key comes from the environment, and a test that needs either is a
 *    `Live` one. The compile holds what the page promises: one
 *    `given Embedder` and one `given Judge.Fit` at the root, and
 *    `Dlm.of` unchanged beneath them.
 *
 * No `import okay.dlm.*` at the top: each block brings its own, as the
 * page does, and an outer one would make the page's line unused.
 */
class TestDocExamplesDlmRemote extends FunSuite:

  /** the stand-in for a model on disk: an encoder of the page's shape */
  object onnx:
    def embed(text: String): okay.rag.Embedding = okay.rag.Vectors.hashing(384)(text)

  /** docs/modules/okay-dlm-remote.md's block */
  def laya(acts: okay.dlm.Exemplars): okay.dlm.Head =
    import okay.dlm.*, okay.dlm.remote.*

    given Judge.Fit = Judge.Fit.constant(Judge.orElse(Laya.judge(), Judge.probe(acts)))
    val head = Head.of(Some(acts), margin = 0.5f, quiet = Some("answer"),
      instructions = "What kind of move is this message?",
      descriptions = Map("social" -> "a pleasantry", "correct" -> "a correction of what we understood"))
    head

  /** docs/modules/okay-dlm.md's composition root, behind a def nobody calls */
  def compositionRoot(intents: okay.dlm.Intents, vectors: okay.dlm.Exemplars,
                      acts: okay.dlm.Exemplars): Either[String, okay.dlm.Dlm] =
    import okay.dlm.*
    import okay.dlm.remote.*
    given Embedder = Embedder.of("minilm-l12", 384, onnx.embed)            // the model on disk
    given Judge.Fit = Judge.Fit.constant(Judge.orElse(                      // Jev first, ours behind it
      Jev.judge(sys.env("TYPESAFE_API_KEY")), Judge.probe(acts)))
    val model = Dlm.of(intents, Some(vectors), heads = Map("acts" -> (acts, 0.5f)))
    model

  test("okay-dlm-remote.md: Laya as the judge, ours behind it — the head builds, and asks nothing yet") {
    val acts = okay.dlm.Exemplars.compile(Vector(
      "answer" -> "Вроцлав", "social" -> "спасибо большое", "correct" -> "ты меня не понял"))
    // building sends nothing: Laya is asked only when the head is
    assertEquals(laya(acts).classes, Vector("answer", "social", "correct"))
  }

  test("okay-dlm.md's composition root compiles against the API as it is, over an encoder of the page's shape") {
    // `compositionRoot` compiling is the assertion; what runs here is the
    // one thing checkable offline, that the stand-in has the page's width
    assertEquals(onnx.embed("спасибо").size, 384)
  }
