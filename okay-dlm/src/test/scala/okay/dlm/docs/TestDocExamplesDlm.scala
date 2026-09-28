package okay.dlm.docs

import munit.FunSuite

/**
 * docs/modules/okay-dlm.md's "In sixty seconds", every line held
 * verbatim and RUN (doc-snippets-pin-all's rule: a doc's Scala example is
 * a line of a compiled source). The page landed with the block unpinned
 * (dlm-module, 2026-09-28) and TestDocSnippets' ratchet went red on
 * master; pinning it changed two lines: the encoder is a real one
 * (`Vectors.hashing`, where the page had `…`), and the artifacts go to a
 * directory the caller names, not `resources/` under the working
 * directory — a test writing into the source tree is the thing a reader
 * would copy.
 *
 * In its own package so `import okay.dlm.*` is the page's import, used.
 * The three bare lines of the page are the last expressions of the three
 * defs below, so each is kept exactly and still asserted.
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

  def routed(model: Dlm) =
    model.router.route("ищю сантехника")        // Fires("need", Map(what -> …), Typo(1))

  def act(model: Dlm) =
    model.head("acts").of("спасибо большое")     // Some("social"), or None: an answer

  def decided(evidence: Decision.Evidence) =
    Decision.decide(State("ann", "ru"), evidence) // Action.Act(route) | AskPlainly | Menu | …

  test("In sixty seconds: the page's block runs, and says what it says") {
    val dir = java.nio.file.Files.createTempDirectory("okay-dlm-doc")
    val intents = Intents.parse(json).toOption.get          // the caller's file
    val embed: String => okay.rag.Embedding = okay.rag.Vectors.hashing(256)   // any encoder; hashing for a test
    val vectors = Exemplars.compile(intents.rows, embed, "minilm-l12")
    Exemplars.write(dir.resolve("intents.vec.json"), vectors)   // JSON and safetensors
    assert(java.nio.file.Files.isRegularFile(dir.resolve("intents.vec.json")))

    val acts = Exemplars.compile(Vector(
      "answer" -> "Вроцлав", "answer" -> "программист на Scala", "answer" -> "пять лет опыта",
      "social" -> "спасибо большое", "social" -> "хорошего дня", "social" -> "благодарю вас",
      "correct" -> "нет, я имел в виду другое", "correct" -> "ты меня не понял", "correct" -> "не так, исправь"),
      embed, "hashing-256")
    val model = Dlm.of(intents, embed, Some(vectors),
      heads = Map("acts" -> (acts, 0.5f)), alphabet = Alphabet.of("ru", "uk", "pl", "en").toOption.get)

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
