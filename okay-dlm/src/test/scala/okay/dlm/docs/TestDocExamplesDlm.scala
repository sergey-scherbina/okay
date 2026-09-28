package okay.dlm.docs

import munit.FunSuite

/**
 * docs/modules/okay-dlm.md, its first two blocks, every line held
 * verbatim and RUN (doc-snippets-pin-all's rule: a doc's Scala example is
 * a line of a compiled source). The third block, a composition root over
 * a model on disk and a paid judge, is okay-dlm-remote's
 * TestDocExamplesDlmRemote, compiled there.
 *
 * The page landed unpinned (dlm-module, 2026-09-28) and TestDocSnippets'
 * ratchet went red on master; the backends lane the same morning then
 * moved `Dlm.of` onto a given `Embedder` returning `Either`, which the
 * first block still called the old way. Pinning it (dlm-doc-pinned)
 * rewrote that block to the API as it is: the table compiled by the
 * encoder in scope, the model taken out of its `Either`, the artifacts
 * written to a directory the caller names rather than under the working
 * directory.
 *
 * In its own package so `import okay.dlm.*` is the page's import, used.
 * The page's three bare lines are the last expressions of three defs, so
 * each is kept exactly and still asserted.
 */
class TestDocExamplesDlm extends FunSuite:
  import okay.dlm.*

  /** the caller's file: two intents in the shape TestRouter builds by hand */
  val json = """{"intents": [
    {"name": "need", "rules": ["(?iU)\\b(?:нужен|нужна|ищу)\\b"],
     "examples": {"ru": ["мне нужен сантехник", "ищу электрика на завтра", "кто починит холодильник"]},
     "slots": [{"name": "what", "pattern": "(?iU)(?:нужен|нужна|ищу)\\s+(.+)", "fallback": true}]},
    {"name": "offer", "rules": ["(?iU)\\b(?:умею|могу|предлагаю)\\b"],
     "examples": {"ru": ["умею писать программы на Scala", "могу починить стиральную машину", "предлагаю услуги электрика"]},
     "slots": [{"name": "what", "pattern": "(?iU)(?:умею|могу|предлагаю)\\s+(.+)", "fallback": true}]}
  ]}"""

  /** the act head's table, by the same (given) encoder */
  val acts = Exemplars.compile(Vector(
    "answer" -> "Вроцлав", "answer" -> "программист на Scala", "answer" -> "пять лет опыта",
    "social" -> "спасибо большое", "social" -> "хорошего дня", "social" -> "благодарю вас",
    "correct" -> "нет, я имел в виду другое", "correct" -> "ты меня не понял", "correct" -> "не так, исправь"))

  def routed(model: Dlm) =
    model.router.route("ищю сантехника")        // Fires("need", Map(what -> …), Typo(1))

  def act(model: Dlm) =
    model.head("acts").of("спасибо большое")     // Some("social"), or None: an answer

  def decided(evidence: Decision.Evidence) =
    Decision.decide(State("ann", "ru"), evidence) // Action.Act(route) | AskPlainly | Menu | …

  test("In sixty seconds: the page's block runs, and says what it says") {
    val dir = java.nio.file.Files.createTempDirectory("okay-dlm-doc")
    val intents = Intents.parse(json).toOption.get          // the caller's file
    val vectors = Exemplars.compile(intents.rows)             // by the Embedder in scope: ours unless a given says otherwise
    Exemplars.write(dir.resolve("intents.vec.json"), vectors)   // JSON and safetensors
    assert(java.nio.file.Files.isRegularFile(dir.resolve("intents.vec.json")))

    val model = Dlm.of(intents, Some(vectors),
      heads = Map("acts" -> (acts, 0.5f)), alphabet = Alphabet.of("ru", "uk", "pl", "en").toOption.get).toOption.get

    routed(model) match
      case Route.Fires("need", slots, Support.Typo(1)) => assert(slots.contains("what"), slots.toString)
      case other => fail(s"the page says Fires(need, what, Typo(1)); it is $other")
    assertEquals(act(model), Some("social"))

    val fired = routed(model)
    val evidence = new Decision.Evidence:
      def route = Some(fired)
      def pendingAct = None
      def courtesy = false
      def hasModel = false
    assertEquals(decided(evidence), Decision.Action.Act(fired))
  }

  test("Backends: ours by default — the line reaches no network and needs no file, and builds the model") {
    val intents = Intents.parse(json).toOption.get
    val vectors = Exemplars.compile(intents.rows)
    val model = Dlm.of(intents, Some(vectors), heads = Map("acts" -> (acts, 0.5f)))
    assert(model.isRight, model.toString)
    // the page's last claim: a table compiled by another encoder is refused by name
    val foreign = Exemplars.compile(intents.rows, okay.rag.Vectors.hashing(64), "minilm-l12")
    assert(Dlm.of(intents, Some(foreign)).left.exists(_.contains("minilm-l12")), "a foreign table was not refused by name")
  }
