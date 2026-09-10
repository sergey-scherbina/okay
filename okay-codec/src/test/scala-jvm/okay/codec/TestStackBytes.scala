package okay.codec

/**
 * What a full-depth decode costs in STACK BYTES (stack-depth-margin),
 * JVM-only because `Thread(group, r, name, stackSize)` is the only API
 * anywhere in this build that lets a measurement choose the stack it
 * runs on — which is what makes this number a property of the DOOR and
 * not of whatever `-Xss` the runner happened to pass.
 *
 * MEASURED 2026-09-10, Java 21 on aarch64, powers of two from 16 KB:
 *
 *   JsonValue.parse      256 KB      Json.readStrict[Tree]   512 KB
 *   Json.lossless        512 KB      Staged.strict[Tree]     512 KB
 *   Json.read[Tree]     1024 KB      Staged.cbor[Tree]      1024 KB
 *   Cbor.read[Tree]     1024 KB
 *
 * The finding that matters, and it was the opposite of what this lane
 * predicted: the tight platform is not the browser, it is a JVM thread
 * with the DEFAULT 1 MB stack, where a full-depth decode of a RECURSIVE
 * schema (~8 KB of stack per tree level) leaves nothing for the caller.
 * sbt's `-Xss8m` and the 8 MB main thread on macOS are what hid it.
 * specs/codecs.md carries the consequence; this test keeps the number
 * from drifting quietly.
 */
class TestStackBytes extends munit.FunSuite:

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def arrays(d: Int): String = ("[" * d) + "1" + ("]" * d)
  def jsonTree(d: Int): String = ("{\"kids\":[" * d) + "{\"kids\":[]}" + ("]}" * d)
  def cborTree(d: Int): Array[Byte] =
    val out = new Cbor.Out
    for _ <- 0 until d do { out.mapHeader(1); out.text("kids"); out.arrayHeader(1) }
    out.mapHeader(1); out.text("kids"); out.arrayHeader(0)
    out.toArray

  val levels = Codecs.maxDepth / 2 - 1

  val doors: List[(String, () => Boolean)] = List(
    "JsonValue.parse"       -> (() => JsonValue.parse(arrays(Codecs.maxDepth)).isDefined),
    "Json.lossless"         -> (() => !Json.isCut(Json.lossless(arrays(Codecs.maxDepth)))),
    "Json.read[Tree]"       -> (() => Json.read[Tree](jsonTree(levels)).isRight),
    "Json.readStrict[Tree]" -> (() => Json.readStrict[Tree](jsonTree(levels)).isRight),
    "Cbor.read[Tree]"       -> (() => Cbor.read[Tree](cborTree(levels)).isRight),
    "Staged.cbor[Tree]"     -> (() => Staged.cbor[Tree].decode(cborTree(levels)).isRight),
    "Staged.strict[Tree]"   -> (() => Staged.strict[Tree].decode(jsonTree(levels)).isRight),
  )

  /** `body` on its own thread with exactly this much stack; None when
    * it ran out of it. An overflow is caught on that thread and nowhere
    * else, which is why this probe cannot take the suite down. */
  def onStack(kb: Int)(body: () => Boolean): Option[Boolean] =
    // no volatile needed: `join` is the happens-before edge
    var out: Option[Boolean] = None
    val t = new Thread(null, () => out =
      try Some(body()) catch case _: StackOverflowError => None,
      s"stack-probe-$kb", kb.toLong * 1024)
    t.start()
    t.join()
    out

  /** the smallest power-of-two stack the door completes on — MAX of 3
    * rounds (bench-one-round-lies): a cold JIT state can need noticeably
    * more than a warm one for the same door, so the min-of-3 that would
    * be right for a PERFORMANCE number is wrong for a SAFETY one here */
  def needs(door: () => Boolean): Int =
    (1 to 3).map { _ =>
      var kb = 16
      while kb <= 8192 && onStack(kb)(door) != Some(true) do kb *= 2
      kb
    }.max

  test("a full-depth decode fits in 2 MB on every door, and the fast parse in 512 KB") {
    val measured = doors.map((name, door) => (name, needs(door)))
    measured.foreach((name, kb) => println(f"[stack] $name%-22s needs $kb%5d KB at depth ${Codecs.maxDepth}"))
    val worst = measured.maxBy(_._2)
    assert(worst._2 <= 2048, s"${worst._1} needs ${worst._2} KB — a default JVM thread has 1024")
    val fast = measured.find(_._1 == "JsonValue.parse").get._2
    assert(fast <= 512, s"the fast value parser needs $fast KB, which it used not to")
  }

  test("the measurement is honest: one level LESS of nesting costs less stack") {
    // the guard against a probe that measures something other than the
    // recursion — if these were equal, the number would not be the
    // door's depth cost at all
    def treeAt(d: Int): () => Boolean = () => Cbor.read[Tree](cborTree(d)).isRight
    val deep = needs(treeAt(levels))
    val shallow = needs(treeAt(8))
    assert(shallow < deep, s"a tree of 8 levels needs $shallow KB and one of $levels needs $deep KB")
    println(f"[stack] Cbor.read[Tree]: $shallow%d KB at 8 levels, $deep%d KB at $levels levels")
  }
