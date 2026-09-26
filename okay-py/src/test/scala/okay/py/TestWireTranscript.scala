package okay.py

import okay.codec.Json
import PyValue.*

/** the golden transcript (`/okay/py/wire.txt`): its hello, and its steps
 * as (what the host sends, what the far side answers) */
object WireTranscript:
  final case class Step(request: Json, answer: Json)

  lazy val lines: Vector[(Char, Json)] =
    val in = getClass.getResourceAsStream("/okay/py/wire.txt")
    val text = try String(in.readAllBytes(), "UTF-8") finally in.close()
    text.linesIterator.toVector.collect {
      case l if l.startsWith("> ") || l.startsWith("< ") => (l.head, Json.parse(l.drop(2)))
    }

  lazy val hello: Json = lines.head._2
  lazy val steps: Vector[Step] = lines.tail.grouped(2).toVector.map {
    case Vector(('>', q), ('<', a)) => Step(q, a)
    case other => throw IllegalStateException(s"the transcript pairs a request with an answer, got $other")
  }

  /** JSON as the transcript compares it: objects by key, numbers by value,
   * an expected "*" matching anything */
  def same(expected: Json, got: Json): Boolean = (expected, got) match
    case (Json.JStr("*"), _) => true
    case (Json.JObj(e), Json.JObj(g)) =>
      val gm = g.toMap
      e.size == g.size && e.forall((k, v) => gm.get(k).exists(same(v, _)))
    case (Json.JArr(e), Json.JArr(g)) => e.size == g.size && e.zip(g).forall(same)
    case (Json.JNum(a), Json.JNum(b)) => a == b
    case (a, b) => a == b

/**
 * The HOST against the transcript, in the default gate: a scripted far side
 * answers with the transcript's `<` lines, the host performs the operations
 * the transcript's comments name, and every request it sends must be the
 * transcript's `>` line.
 */
class TestWireTranscript extends munit.FunSuite:
  import WireTranscript.*

  test("the host sends exactly the transcript's requests") {
    val sent = Vector.newBuilder[Json]
    var next = 0
    val link = new WireLink:
      def hello(): Option[String] = Some(Json.print(WireTranscript.hello))
      def roundTrip(line: String): Option[String] =
        sent += Json.parse(line)
        val a = steps(next).answer
        next += 1
        Some(Json.print(a))
      def exchange(message: Array[Byte]): Option[Array[Byte]] = None
      def close(): Unit = ()
    val w = ForeignWorker.over(link, "the transcript")
    val h = w.handler
    val ref = PyRef(1, "t.Counter")
    h.handle(ForeignEval.Call("t:add", Vector(I64(2), I64(3)))): Unit
    h.handle(ForeignEval.Call("t:counter", Vector(I64(10)), held = true)): Unit
    h.handle(ForeignEval.Call(Address.Method(ref, "inc"), Vector(I64(5)))): Unit
    h.handle(ForeignEval.Call(Address.Attr(ref, "n"), Vector.empty)): Unit
    h.handle(ForeignEval.Frame("t:scale", PyFrame(Vector("x" -> Vector(I64(1), I64(2)))), Vector(I64(3)))): Unit
    h.handle(ForeignEval.Program(1, "t:pairs", Vector.empty, Vector("choose"))): Unit
    h.handle(ForeignEval.Continue(1, 1, Right(I64(1)))): Unit
    h.handle(ForeignEval.Continue(1, 1, Right(I64(2)))): Unit
    h.handle(ForeignEval.Forget(1))
    h.handle(ForeignEval.Program(2, "t:quote", Vector(Str("tea")), Vector("price_of"), direct = true)): Unit
    h.handle(ForeignEval.Continue(2, 1, Right(F64(4.25)))): Unit
    h.handle(ForeignEval.Call("t:boom", Vector.empty)): Unit
    h.handle(ForeignEval.Release(ref))
    h.handle(ForeignEval.Call(Address.Attr(ref, "n"), Vector.empty)): Unit
    val got = sent.result()
    assertEquals(got.size, steps.size, "one request per step")
    steps.zip(got).zipWithIndex.foreach { case ((s, g), i) =>
      assert(same(s.request, g), s"step ${i + 1}: the transcript says\n  ${Json.print(s.request)}\nthe host sent\n  ${Json.print(g)}")
    }
  }

/**
 * The REFERENCE far side against the transcript (Live): the Python shim,
 * serving the transcript's module `t`, answers every `>` line with exactly
 * its `<` line — the check a new language's library is written against.
 */
class TestWireTranscriptPython extends munit.FunSuite:
  import WireTranscript.*

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  private val t = Foreign.module("t", """
    import okay
    from okay import okay_call
    def add(a, b): return a + b
    class Counter:
        def __init__(self, n): self.n = n
        def inc(self, by): self.n += by; return self.n
    def counter(n): return Counter(n)
    def scale(frame, k): return {"x": [v * k for v in frame["x"]]}
    def pairs(): return okay.perform("choose", [1, 2]).then(lambda x: okay.done(x * 10))
    def quote(sku): return okay_call("price_of", sku) * 2
    def boom(): raise ValueError("no")
  """)

  test("the Python shim answers exactly the transcript's answers") {
    val c = ForeignWorker.pythonCommand(TestPy.python.get, Seq(t))
    val pb = ProcessBuilder(c.command*)
    pb.environment().clear()
    c.env.foreach((k, v) => pb.environment().put(k, v))
    val p = pb.start()
    try
      val in = java.io.BufferedReader(java.io.InputStreamReader(p.getInputStream, "UTF-8"))
      val out = java.io.PrintStream(p.getOutputStream, true, "UTF-8")
      // the hello's version must match; what it speaks depends on the box
      // (pyarrow installed or not), so only the version is the transcript's
      val hello = Json.parse(in.readLine())
      def shim(j: Json) = j match
        case Json.JObj(fs) => fs.toMap.get("shim")
        case _ => None
      assertEquals(shim(hello), shim(WireTranscript.hello), s"the hello: ${Json.print(hello)}")
      steps.zipWithIndex.foreach { (s, i) =>
        out.println(Json.print(s.request))
        val got = Json.parse(in.readLine())
        assert(same(s.answer, got), s"step ${i + 1}: the transcript says\n  ${Json.print(s.answer)}\nthe shim answered\n  ${Json.print(got)}")
      }
    finally p.destroy()
  }
