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
 *
 * THOSE KILOBYTES ARE HOST-RELATIVE, and this file now says so in
 * code rather than in a comment nobody reads: a thread's stackSize is
 * a REQUEST, a JVM floors it at whatever its guard zone leaves
 * usable. Measured 2026-09-19 on aarch64 Linux in a container, JDK
 * 21.0.10, 4 KB pages, StackShadowPages=20: requests of 8, 16, 24,
 * 32, 48, 64, 96 and 128 KB ALL gave 876 frames of plain recursion —
 * one stack of ~34 KB usable, 80 KB of shadow inside a ~128 KB
 * minimum — while 160/192/224/256 gave 1490/2308/3128/3948. A ladder
 * of powers of two has ONE rung below 256 there, so a door needing a
 * little over the floor can report nothing between them, and a door
 * sitting ON the floor flaps. On the machine those numbers were taken on the floor is at
 * or below 16 KB; on a container measured 2026-09-19 it was 128, and
 * the old fixed ladder reported 256 KB for a door that is in fact
 * flat. See `needsWarm` for the other half — the cold round answers a
 * different question from the flat one — and okay-codec/BUGS.md
 * stackbytes-probe-measures-the-hosts-floor for the whole
 * measurement.
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

  /** the rungs, and the sentinel above them: 16384 means "not even
    * 8 MB did", which is what the `while` loop this replaced returned */
  val ladder: List[Int] = Iterator.iterate(16)(_ * 2).takeWhile(_ <= 8192).toList

  /** the smallest rung the door completes on — MAX of 3 rounds
    * (bench-one-round-lies): a cold JIT state can need noticeably
    * more than a warm one for the same door, so the min-of-3 that
    * would be right for a PERFORMANCE number is wrong for a SAFETY
    * one here */
  def needs(door: () => Boolean): Int =
    (1 to 3).map(_ => ladder.find(kb => onStack(kb)(door) == Some(true)).getOrElse(16384)).max

  /**
   * The same, once the door is WARM — which is a different question
   * and belongs to a different test.
   *
   * "How much stack can this door ever need" wants the cold round and
   * gets it above. "Does the trampoline engage" is about the SHAPE of
   * the code, and an interpreted frame is several times a compiled
   * one, so comparing a cold deep door against a warm shallow one
   * measures the JIT rather than the trampoline. Measured on the same
   * container: cold, depth 8 answered 256, 16, 16, 16, 16 KB across
   * five rounds; warm, depths 8, 100, 200 and 400 all answered 16 KB
   * in every round.
   */
  def needsWarm(door: () => Boolean, rounds: Int = 2000): Int =
    val t = new Thread(null, () => { var i = 0; while i < rounds do { door(): Unit; i += 1 } },
      "warmup", 8L * 1024 * 1024)
    t.start(); t.join()
    // MIN of 3, where `needs` takes the max, and the asymmetry is the
    // point. A door sitting near the floor of what a host will give a
    // thread flaps: measured here, the SHALLOW door answered 64 KB in
    // a round where the deep one answered 16, which is not a thing
    // that can be true. For "what can this door ever need" a flap is
    // the answer you must keep; for "does the trampoline engage" it
    // is noise about the host, and the settled round is the evidence.
    (1 to 3).map(_ => ladder.find(kb => onStack(kb)(door) == Some(true)).getOrElse(16384)).min

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
      // WARM: the question here is the shape of the code, not the
      // state of the JIT (see needsWarm)
      val shallow = needsWarm(doorAt(8))
      val deep = needsWarm(doorAt(levels))
      val deeper = needsWarm(doorAt(levels * 4))
      println(f"[stack] $name%-16s $shallow%d KB at 8 levels, $deep%d KB at $levels, $deeper%d KB at ${levels * 4}")

      // THE TWO DEPTHS COMPARED ARE BOTH PAST THE THRESHOLD, and
      // that is the correction this lane made. The old comparison was
      // 8 levels against `levels`, which is the NATIVE path against
      // the trampolined one — different code, and on a host with fat
      // frames the native one can legitimately cost MORE (measured
      // 2026-09-19: Cbor.read wanted 32 KB at 8 levels and 16 at 100,
      // and the test called that a failure to flatten). What the two
      // trampoline lanes actually promise is that past the threshold
      // the cost stops following the depth — so ask that.
      assertEquals(deeper, deep,
        s"$name: ${levels * 4} levels cost more than $levels — past the threshold the cost still follows the depth")
      assert(deep <= shallow,
        s"$name: $deep KB past the threshold against $shallow KB below it — the trampoline costs more than the recursion it replaced")
  }
