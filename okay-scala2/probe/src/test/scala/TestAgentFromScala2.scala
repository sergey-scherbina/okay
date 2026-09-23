package scala2probe

import okay.agent.Turn
import okay.codec.Schema
import okay.scala2._

object AgentModel {
  final case class SearchArgs(query: String, limit: Option[Int])
  object SearchArgs {
    implicit val schema: Schema[SearchArgs] =
      Schemas.product2("SearchArgs", "query", "limit")(SearchArgs.apply)(a => (a.query, a.limit))
  }

  val searched = scala.collection.mutable.ListBuffer.empty[SearchArgs]

  val tools: Tools = Tools.empty.on[SearchArgs]("search", "look something up") { a =>
    searched += a
    s"${a.limit.getOrElse(10)} hits for '${a.query}'"
  }
}

/** okay-agent from Scala 2.13 (specs/scala2-facade.md, stage 9) */
class TestAgentFromScala2 extends munit.FunSuite {
  import AgentModel._

  def results(chat: Chat): Seq[String] = chat.transcript.collect { case Turn.Result(_, text) => text }

  test("a chat answers, and the conversation carries over to the next message") {
    val chat = Chat(Model.scripted("hello, ada", "you said your name was ada"), policy = Policy.all)
    assertEquals(Eff.runAsync(chat.say("hi, I am ada")), "hello, ada")
    assertEquals(Eff.runAsync(chat.say("what is my name?")), "you said your name was ada")
    assertEquals(chat.transcript.collect { case Turn.User(t) => t }, Seq("hi, I am ada", "what is my name?"))
  }

  test("the model calls a tool; its arguments arrive as a case class; the result goes back to the model") {
    searched.clear()
    val model = Model.scriptedCalls(
      ("let me look", Seq("search" -> """{"query":"okay","limit":3}""")),
      ("found 3 hits", Seq.empty))
    val chat = Chat(model, tools, Policy.all)
    assertEquals(Eff.runAsync(chat.say("find okay")), "found 3 hits")
    assertEquals(searched.toList, List(SearchArgs("okay", Some(3))))
    assertEquals(results(chat), Seq("3 hits for 'okay'"))
  }

  test("approve can deny a call; the model is told, the tool never runs") {
    searched.clear()
    val model = Model.scriptedCalls(("", Seq("search" -> """{"query":"secrets"}""")), ("ok", Seq.empty))
    var asked: List[Call] = Nil
    val chat = Chat(model, tools, Policy.all, approve = c => { asked = c :: asked; false })
    assertEquals(Eff.runAsync(chat.say("look up the secrets")), "ok")
    assertEquals(asked.map(c => (c.name, c.argsJson)), List(("search", """{"query":"secrets"}""")))
    assertEquals(results(chat), Seq("denied"))
    assert(searched.isEmpty)
  }

  test("the tool declarations carry the arguments' JSON Schema") {
    assertEquals(tools.declarations.size, 1)
    val (name, description, schema) = tools.declarations.head
    assertEquals((name, description), ("search", "look something up"))
    assert(schema.contains("\"query\""), schema)
  }

  test("a window policy keeps the conversation within budget and reports what it dropped") {
    val long = "x" * 200
    val chat = Chat(Model.scripted(Seq.fill(6)(long): _*), policy = Policy.window(120))
    (1 to 6).foreach(i => Eff.runAsync(chat.say("turn " + i + " " + long)))
    val t = chat.transcript
    assert(t.exists { case Turn.Summary(s, n) => n > 0 && s.contains("elided"); case _ => false }, t.toString)
    assert(t.size < 12, t.toString)
  }
}

/** a real model, when a key is present: tagged Live and skipped without one */
class TestAgentLiveFromScala2 extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  test("a real model answers through Chat") {
    val key = sys.env.get("ANTHROPIC_API_KEY")
    assume(key.isDefined, "no ANTHROPIC_API_KEY")
    val chat = Chat(Model.anthropic(key.get, "claude-haiku-4-5-20251001", maxTokens = 64))
    val answer = Eff.runAsync(chat.say("Reply with exactly the word: pong"))
    assert(answer.toLowerCase.contains("pong"), answer)
  }
}
