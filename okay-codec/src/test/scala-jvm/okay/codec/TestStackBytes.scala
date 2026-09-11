package okay.codec

/**
 * What a full-depth decode costs in STACK BYTES (stack-depth-margin),
 * JVM-only because `Thread(group, r, name, stackSize)` is the only API
 * anywhere in this build that lets a measurement choose the stack it
 * runs on — which is what makes this number a property of the DOOR and
 * not of whatever `-Xss` the runner happened to pass.
 *
 * MEASURED 2026-09-10, Java 21 on aarch64, BEFORE either threshold
 * lane, at `Codecs.maxDepth` = 64 (a correction: an earlier version of
 * this comment carried "512 KB" for the two `JsonStrict`-based doors,
 * copied forward from a measurement taken at the OLDER `maxDepth` =
 * 256 without re-measuring at 64 — checked here by re-running against
 * the untouched `Json.scala` before writing this sentence, per
 * verify-assumptions-before-acting):
 *
 *   JsonValue.parse        16 KB      Json.readStrict[Tree]   16 KB
 *   Json.lossless          16 KB      Staged.strict[Tree]     16 KB
 *   Json.read[Tree]       256 KB      Staged.cbor[Tree]      256 KB
 *   Cbor.read[Tree]       256 KB
 *
 * At `Codecs.maxDepth` = 64, only the two doors that fold a RECURSIVE
 * schema through `Json.decode`/`Cbor.get` (`Json.read[Tree]`,
 * `Cbor.read[Tree]`, and `Staged.cbor[Tree]` which falls back to
 * `Cbor.get` for the recursive case) show a real cost at this shallow
 * a depth; the `JsonStrict`-based doors' per-level cost is genuinely
 * lower and does not show up until deeper than `Codecs.maxDepth`
 * allows constructing (stack-depth-margin's ORIGINAL 512 KB figure for
 * them was measured at maxDepth=256's full 127 levels, four times this
 * deep).
 *
 * AFTER cbor-decode-threshold-trampoline AND json-decode-threshold-
 * -trampoline (both same day): every door in the list is flat at
 * 16 KB — both recursive-schema roots of
 * `specs/iterative-recursive-decode.md` are closed. `JsonStrict.
 * Reader.get` was never the third root this file's history implied;
 * it did not need fixing at this depth.
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

  /** comfortably past NativeThreshold; two containers per tree level */
  val levels = 100
  val rawDepth = 200

  val doors: List[(String, () => Boolean)] = List(
    "JsonValue.parse"       -> (() => JsonValue.parse(arrays(rawDepth)).isDefined),
    "Json.lossless"         -> (() => !Json.lossless(arrays(rawDepth)).isInstanceOf[Json.JErr]),
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

  test("every door fits in 2 MB at a full-depth document") {
    val measured = doors.map((name, door) => (name, needs(door)))
    measured.foreach((name, kb) => println(f"[stack] $name%-22s needs $kb%5d KB at depth $levels"))
    val worst = measured.maxBy(_._2)
    assert(worst._2 <= 2048, s"${worst._1} needs ${worst._2} KB — a default JVM thread has 1024")
  }

  test("the measurement is honest: the probe itself, decoupled from any door") {
    // this USED TO read a door's own cost at two depths (first
    // Cbor.read[Tree], then Json.read[Tree]) — fragile, because fixing
    // a door (the whole point of this file's two lanes) retires it as
    // a witness, and this test then measures nothing. A synthetic,
    // non-tail recursion — nothing to do with Schema or codecs —
    // proves `onStack`/`needs` measure real stack, permanently, no
    // matter which door in this file is fixed next
    def burn(n: Int): Int = if n <= 0 then 1 else 1 + burn(n - 1) - 1
    def at(n: Int): () => Boolean = () => burn(n) == 1
    val deep = needs(at(200000))
    val shallow = needs(at(8))
    assert(shallow < deep, s"200 000 frames of plain recursion needs $shallow KB, same as 8 — the probe measures nothing")
    println(f"[stack] synthetic recursion: $shallow%d KB at 8 frames, $deep%d KB at 200 000 frames")
  }

  test("both threshold lanes: every door is flat past the threshold") {
    // the combined signature of cbor-decode-threshold-trampoline and
    // json-decode-threshold-trampoline: at 8 levels (well below
    // NativeThreshold) and at `levels` (comfortably past it,
    // crossing it), every recursive-schema door costs the SAME —
    // proving the switch happens, not just compiles, for both roots
    for (name, doorAt) <- List(
      "Json.read[Tree]" -> ((d: Int) => () => Json.read[Tree](jsonTree(d)).isRight),
      "Cbor.read[Tree]" -> ((d: Int) => () => Cbor.read[Tree](cborTree(d)).isRight),
    ) do
      val shallow = needs(doorAt(8))
      val deep = needs(doorAt(levels))
      println(f"[stack] $name%-16s $shallow%d KB at 8 levels, $deep%d KB at $levels levels")
      assertEquals(deep, shallow, s"$name: expected the trampoline to flatten the cost past the threshold")
      assert(deep <= 64, s"$name at $levels levels needs $deep KB — the trampoline should cost near nothing")
  }
