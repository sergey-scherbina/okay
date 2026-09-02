package okay.mcp

import okay.*
import okay.given
import okay.agent.{Handlers, Reply, ToolCall, ToolSpec, Turn}
import okay.codec.{Json, Schema}

/**
 * The duplex half: the server talking first. Subscriptions, roots,
 * sampling — and the interleaving that makes all three one problem.
 */
class TestDuplex extends munit.FunSuite {

  final case class Add(a: Int, b: Int)
  given Schema[Add] = Schema.derived

  val info = Mcp.Info("okay-mcp", "0.1")
  val spec = ToolSpec[Add]("add", "add two numbers")

  def wire(): (Link, Link) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  /** a served server, and the handle for what it says unasked */
  def served(serving: Server.Serving, peer: Duplex.Peer = Duplex.Peer())
  : (Session, Server.Pushes) =
    val (client, server) = wire()
    val (prog, pushes) = Server.duplex(server, serving)
    Async.spawn(prog): Unit
    (Client.connect(client, Mcp.Info("test", "1"), peer).runWith, pushes)

  val docs = Server.Serving(info,
    resources = Seq(Mcp.Resource("okay://a", "a"), Mcp.Resource("okay://b", "b")),
    read = Map("okay://a" -> "alpha", "okay://b" -> "beta").get)

  test("a subscription delivers updates for that uri and no other") {
    val (s, pushes) = served(docs)
    assert(s.subscribe("okay://a").runWith)

    pushes.resourceUpdated("okay://b")   // nobody subscribed to b
    pushes.resourceUpdated("okay://a")
    pushes.listChanged(Mcp.ResourcesChanged)   // the sentinel: everything before it has been sent

    var seen = List.empty[Rpc.Notify]
    var done = false
    while !done do
      val n = s.notifications.receiveBlocking().get
      if n.method == Mcp.ResourcesChanged then done = true else seen = seen :+ n

    assertEquals(seen.map(_.method), List(Mcp.ResourceUpdated))
    assertEquals(seen.flatMap(Duplex.updatedUri), List("okay://a"))
  }

  test("unsubscribe stops them") {
    val (s, pushes) = served(docs)
    assert(s.subscribe("okay://a").runWith)
    assert(s.unsubscribe("okay://a").runWith)

    pushes.resourceUpdated("okay://a")
    pushes.listChanged(Mcp.ResourcesChanged)
    assertEquals(s.notifications.receiveBlocking().get.method, Mcp.ResourcesChanged)
  }

  test("a notification arriving while the client is IDLE is delivered") {
    val (s, pushes) = served(docs)
    assert(s.subscribe("okay://a").runWith)
    // nothing is asked of the server from here on: the reader is a
    // fiber, not a side effect of asking something
    Thread.sleep(30)
    pushes.resourceUpdated("okay://a")
    assertEquals(Duplex.updatedUri(s.notifications.receiveBlocking().get), Some("okay://a"))
  }

  test("a server asks for roots, and the client answers its own") {
    val roots = Seq(Mcp.Root("file:///work", "work"))
    val asked = Channel[Seq[Mcp.Root]]()
    // a server stage that ASKS: a stage may tell a Request, and the
    // answer arrives back as ordinary input
    val stage: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(info)))
          .flatMap(_ => Stage.tell[Rpc, Rpc](
            Rpc.Request(Json.JStr("r1"), Mcp.RootsList, Rpc.obj())))
      case Rpc.Answer(Json.JStr("r1"), result) =>
        pure(asked.offer(Duplex.rootsOf(result)): Unit)
      case _ => pure(())
    }, pure)

    val (client, server) = wire()
    Async.spawn(Server.over(server)(stage)): Unit
    val s = Client.connect(client, Mcp.Info("test", "1"),
      Duplex.Peer(roots = roots)).runWith
    assertEquals(asked.receiveBlocking(), Some(roots))
    // and telling the server they changed is a notification
    s.rootsChanged.runWith
  }

  test("sampling IS the Model effect: the server borrows the client's model") {
    val answered = Channel[Reply]()
    val stage: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(info)))
          .flatMap(_ => Stage.tell[Rpc, Rpc](Rpc.Request(Json.JStr("s1"), Mcp.SamplingCreate,
            Duplex.samplingParams(Seq(Turn.System("Be terse."), Turn.User("2+2?"))))))
      case Rpc.Answer(Json.JStr("s1"), result) =>
        pure(answered.offer(Duplex.replyOf(result)): Unit)
      case Rpc.Failed(Json.JStr("s1"), _, m) => pure(answered.offer(Reply(s"refused: $m", Nil)): Unit)
      case _ => pure(())
    }, pure)

    // the client's model is the SAME handler an agent would use
    val model = Handlers.scripted(Seq(Reply("4", Nil)))
    val (client, server) = wire()
    Async.spawn(Server.over(server)(stage)): Unit
    val _ = Client.connect(client, Mcp.Info("test", "1"),
      Duplex.Peer(sample = Some(model))).runWith
    assertEquals(answered.receiveBlocking().map(_.text), Some("4"))
  }

  test("a client with no model refuses sampling rather than hanging") {
    val answered = Channel[String]()
    val stage: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(info)))
          .flatMap(_ => Stage.tell[Rpc, Rpc](Rpc.Request(Json.JStr("s1"), Mcp.SamplingCreate,
            Duplex.samplingParams(Seq(Turn.User("hi"))))))
      case Rpc.Failed(Json.JStr("s1"), code, _) => pure(answered.offer(s"refused $code"): Unit)
      case Rpc.Answer(Json.JStr("s1"), _) => pure(answered.offer("answered"): Unit)
      case _ => pure(())
    }, pure)

    val (client, server) = wire()
    Async.spawn(Server.over(server)(stage)): Unit
    val _ = Client.connect(client, Mcp.Info("test", "1")).runWith
    assertEquals(answered.receiveBlocking(), Some(s"refused ${Rpc.MethodNotFound}"))
  }

  test("answers, requests and notifications interleave on one wire") {
    // the server asks the client to sample DURING a tools/call, and
    // both the sampling answer and the call's own answer arrive
    val stage: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, _) =>
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(info)))
      case Rpc.Request(id, Mcp.ToolsCall, _) =>
        // ask first, answer after — the client must handle both
        Stage.tell[Rpc, Rpc](Rpc.Request(Json.JStr("s1"), Mcp.SamplingCreate,
          Duplex.samplingParams(Seq(Turn.User("during"))))).flatMap(_ =>
          Stage.tell[Rpc, Rpc](Rpc.Notify(Mcp.Progress, Rpc.obj())).flatMap(_ =>
            Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.contentResult("3")))))
      case _ => pure(())
    }, pure)

    val (client, server) = wire()
    Async.spawn(Server.over(server)(stage)): Unit
    val s = Client.connect(client, Mcp.Info("test", "1"),
      Duplex.Peer(sample = Some(Handlers.scripted(Seq(Reply("sampled", Nil)))))).runWith

    assertEquals(s.call(ToolCall("c1", "add", Json.JObj(Vector.empty))).runWith, "3")
    assertEquals(s.notifications.receiveBlocking().map(_.method), Some(Mcp.Progress))
  }

  test("a client declares ITS capabilities, so a server knows what to ask") {
    val seen = Channel[Json]()
    val stage: Stage[Rpc, Rpc, Unit] = Stage.transduce(())((_, msg: Rpc) => msg match {
      case Rpc.Request(id, Mcp.Initialize, params) =>
        seen.offer(params): Unit
        Stage.tell[Rpc, Rpc](Rpc.Answer(id, Mcp.initializeResult(info)))
      case _ => pure(())
    }, pure)

    val (client, server) = wire()
    Async.spawn(Server.over(server)(stage)): Unit
    val _ = Client.connect(client, Mcp.Info("test", "1"),
      Duplex.Peer(roots = Seq(Mcp.Root("file:///w")),
        sample = Some(Handlers.scripted(Nil)))).runWith
    val params = seen.receiveBlocking().get
    assert(Mcp.capability(params, "roots"), Json.print(params))
    assert(Mcp.capability(params, "sampling"), Json.print(params))
  }
}
