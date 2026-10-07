package okay.llm

import okay.{Async}

import okay.freer.{%, +, Pure}
import okay.freer.{!, effect, pure}
import okay.std.{Writer}
import okay.given
import okay.freer.given
import okay.llm.Models.*

/** a transport that records what it was asked and answers by the path —
 * every claim of specs/llm-models.md, with no network */
final class ScriptedWire(answers: PartialFunction[(String, String), String]) extends Transport with Fetch:
  val calls = scala.collection.mutable.ListBuffer.empty[(String, String, String)]  // verb, url, body
  private def answer(verb: String, url: String, body: String): Unit ! Writer % String + Async =
    type F = Writer % String + Async
    calls += ((verb, url, body))
    val path = url.substring(url.indexOf('/', 8))
    val text = answers.lift((verb, path)).getOrElse("""{"error":{"message":"no such route","type":"not_found"}}""")
    def go(ls: List[String]): Unit ! F = ls match
      case Nil => pure(())
      case l :: t => effect[F, Unit](Writer(l)).flatMap(_ => go(t))
    go(text.split('\n').toList)
  def post(url: String, headers: Map[String, String], body: String) = answer("POST", url, body)
  def get(url: String, headers: Map[String, String]) = answer("GET", url, "")
  def delete(url: String, headers: Map[String, String], body: String) = answer("DELETE", url, body)

class TestModels extends munit.FunSuite:
  def go[A](p: A ! Async): A = Async.run[A, Pure](p).runWith
  def collect[A](p: Unit ! Writer % A + Async): Seq[A] = go(Writer.run[A, Unit, Async](p))._1

  test("ModelId: org:repo, org/repo and hf:org/repo are one model; show is the canonical form; Ollama's name:tag keeps its spelling") {
    assert(ModelId.same("mlx-community:Qwen3.5-4B-MLX-4bit", "hf:mlx-community/Qwen3.5-4B-MLX-4bit"))
    assertEquals(ModelId("mlx-community:Qwen3.5-4B-MLX-4bit"), ModelId("mlx-community/Qwen3.5-4B-MLX-4bit"))
    assertEquals(ModelId.show(ModelId("hf:mlx-community/Qwen3.5-4B-MLX-4bit")), "mlx-community/Qwen3.5-4B-MLX-4bit")
    assertEquals(ModelId.show(ModelId("mlx-community:Qwen3.5-4B-MLX-4bit")), "mlx-community/Qwen3.5-4B-MLX-4bit")
    assertEquals(ModelId("llama3:8b").spelling, "llama3:8b")
    assertEquals(Set(ModelId("a:b"), ModelId("a/b")).size, 1)
    assert(ModelId("claude-opus-5-5") != ModelId("claude-opus-5"))
  }

  test("openAi: the list form, limits None; info finds a model under another spelling") {
    val w = ScriptedWire { case ("GET", "/v1/models") =>
      """{"object":"list","data":[{"id":"gpt-5","object":"model","created":1700000000,"owned_by":"openai"},{"id":"org/repo","object":"model","owned_by":"x"}]}""" }
    val c = Models.openAi(w, "sk-test")
    val ms = go(c.list)
    assertEquals(ms.map(_.id.spelling), Vector("gpt-5", "org/repo"))
    assertEquals(ms.head, Model(ModelId("gpt-5"), Some("openai"), Some(1700000000L), None, None, Set.empty))
    assertEquals(go(c.info(ModelId("org:repo"))).map(_.id.spelling), Some("org/repo"))
    assertEquals(w.calls.head._2, "https://api.openai.com/v1/models")
  }

  test("anthropic: max_input_tokens, max_tokens and capabilities are carried; pagination is followed") {
    var page = 0
    val w = ScriptedWire { case ("GET", p) if p.startsWith("/v1/models") =>
      page += 1
      if !p.contains("after_id") then
        """{"data":[{"id":"claude-opus-5-5","display_name":"Claude Opus 5.5","created_at":"2026-04-01T00:00:00Z","max_input_tokens":1000000,"max_tokens":128000,"capabilities":["vision","tools"]}],"has_more":true,"first_id":"claude-opus-5-5","last_id":"claude-opus-5-5"}"""
      else
        """{"data":[{"id":"claude-haiku-4-5","capabilities":{"vision":true,"pdf":false}}],"has_more":false,"last_id":"claude-haiku-4-5"}""" }
    val ms = go(Models.anthropic(w, "sk-ant").list)
    assertEquals(ms.map(_.id.spelling), Vector("claude-opus-5-5", "claude-haiku-4-5"))
    assertEquals(ms.head.contextTokens, Some(1000000))
    assertEquals(ms.head.maxOutput, Some(128000))
    assertEquals(ms.head.capabilities, Set("vision", "tools"))
    assertEquals(ms(1).capabilities, Set("vision"))
    assertEquals(page, 2)
    assert(w.calls(1)._2.contains("after_id=claude-opus-5-5"))
  }

  test("a hosted provider is a Catalog and nothing else: .load does not compile") {
    val e = compileErrors("""
      val w = ScriptedWire(PartialFunction.empty)
      Models.openAi(w, "k").load(ModelId("x"))
    """)
    assert(e.contains("load"), e)
    val e2 = compileErrors("""
      val w = ScriptedWire(PartialFunction.empty)
      Models.anthropic(w, "k").unload(ModelId("x"))
    """)
    assert(e2.contains("unload"), e2)
  }

  test("rozum: the resident row is called by its real spec and marked; running reads that mark; load posts /control/switch, unload /control/unload") {
    val w = ScriptedWire {
      case ("GET", "/v1/models") =>
        """{"object":"list","data":[{"id":"claude-rozum-mlx-community-Qwen3-5-4B-MLX-4bit","object":"model","created":1,"owned_by":"rozum","display_name":"mlx-community:Qwen3.5-4B-MLX-4bit","resident":true,"size_bytes":3000000000},{"id":"mlx-community:Qwen3.8-27B-4bit","object":"model","owned_by":"rozum","resident":false,"size_bytes":15000000000}]}"""
      case ("POST", "/control/switch") => """{"status":"switched","model":"mlx-community:Qwen3.8-27B-4bit","generation":2}"""
      case ("POST", "/control/unload") => """{"status":"unloaded","generation":3}""" }
    val r = Models.rozum(w, "http://127.0.0.1:8080")
    val ms = go(r.list)
    assertEquals(ms.map(_.id.spelling), Vector("mlx-community:Qwen3.5-4B-MLX-4bit", "mlx-community:Qwen3.8-27B-4bit"))
    assertEquals(ms.head.capabilities, Set("resident"))
    assertEquals(go(r.running), Vector(Resident(ModelId("mlx-community/Qwen3.5-4B-MLX-4bit"), Some(3000000000L), None)))
    // the resident one, under the catalog's other spelling, is the same model
    assert(ms.exists(m => m.id == go(r.running).head.id && m.capabilities("resident")))
    assertEquals(go(r.load(ModelId("mlx-community:Qwen3.8-27B-4bit"))), Right(()))
    assertEquals(w.calls.last._3, """{"model":"mlx-community:Qwen3.8-27B-4bit"}""")
    assertEquals(go(r.unload(ModelId("x"))), Right(()))
  }

  test("rozum: a refused switch carries the gateway's words; a route that is not there is a refusal, not a throw") {
    val w = ScriptedWire { case ("POST", "/control/switch") => """{"error":{"message":"admission refused: 27B does not fit beside 4B","type":"switch_failed"}}""" }
    val r = Models.rozum(w, "http://h:1")
    assertEquals(go(r.load(ModelId("big"))), Left(Refused("switch", 0, "admission refused: 27B does not fit beside 4B")))
    assertEquals(go(r.unload(ModelId("big"))), Left(Refused("unload", 0, "no such route")))
  }

  test("ollama: tags are local weights, ps is residency with expires_at, load/unload are keep_alive, pull streams progress, delete deletes") {
    val w = ScriptedWire {
      case ("GET", "/v1/models") => """{"object":"list","data":[{"id":"llama3:8b","object":"model","owned_by":"library"}]}"""
      case ("GET", "/api/tags") => """{"models":[{"name":"llama3:8b","model":"llama3:8b","size":4661224676,"digest":"abc"}]}"""
      case ("GET", "/api/ps") => """{"models":[{"name":"llama3:8b","model":"llama3:8b","size":5137025024,"size_vram":5137025024,"expires_at":"2024-06-04T14:38:31.83753-07:00"}]}"""
      case ("POST", "/api/generate") => """{"model":"llama3:8b","done":true,"done_reason":"unload"}"""
      case ("POST", "/api/pull") => "{\"status\":\"pulling manifest\"}\n{\"status\":\"pulling 1\",\"total\":100,\"completed\":40}\n{\"status\":\"pulling 1\",\"total\":100,\"completed\":100}\n{\"status\":\"success\"}"
      case ("DELETE", "/api/delete") => "" }
    val o = Models.ollama(w)
    assertEquals(go(o.list).map(_.id.spelling), Vector("llama3:8b"))
    assertEquals(go(o.local), Vector(Weights(ModelId("llama3:8b"), Some(4661224676L), Some("abc"))))
    val rs = go(o.running)
    assertEquals(rs.head.id.spelling, "llama3:8b")
    assertEquals(rs.head.memoryBytes, Some(5137025024L))
    assertEquals(rs.head.expiresAt, Some(1717537111837L))
    assertEquals(go(o.load(ModelId("llama3:8b"))), Right(()))
    assertEquals(w.calls.last._3, """{"model":"llama3:8b","keep_alive":"5m"}""")
    assertEquals(go(o.unload(ModelId("llama3:8b"))), Right(()))
    assertEquals(w.calls.last._3, """{"model":"llama3:8b","keep_alive":0}""")
    assertEquals(collect(o.pull(ModelId("llama3:8b"))), Seq(Progress.Bytes(40, Some(100)), Progress.Bytes(100, Some(100)), Progress.Done))
    assertEquals(go(o.remove(ModelId("llama3:8b"))), Right(()))
    assertEquals(w.calls.last._1, "DELETE")
  }

  test("ollama: a failed pull tells Failed with the daemon's line") {
    val w = ScriptedWire { case ("POST", "/api/pull") => "{\"status\":\"pulling manifest\"}\n{\"error\":\"pull model manifest: file does not exist\"}" }
    assertEquals(collect(Models.ollama(w).pull(ModelId("nope"))), Seq(Progress.Failed("pull model manifest: file does not exist")))
  }

  test("Iso.epochMs: Z, an offset, a fraction, and garbage") {
    assertEquals(Models.Iso.epochMs("1970-01-01T00:00:00Z"), Some(0L))
    assertEquals(Models.Iso.epochMs("2000-03-01T00:00:00Z"), Some(951868800000L))
    assertEquals(Models.Iso.epochMs("2024-06-04T14:38:31.83753-07:00"), Some(1717537111837L))
    assertEquals(Models.Iso.epochMs("2024-06-04T21:38:31.5+00:00"), Some(1717537111500L))
    assertEquals(Models.Iso.epochMs("soon"), None)
  }
