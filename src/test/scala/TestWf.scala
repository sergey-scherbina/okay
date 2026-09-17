package okay

/**
 * THE LIBRARY'S OWN QUESTIONS (dialogue-asks, 2026-09-17). A durable
 * program needs a clock, an id and sometimes a die; `Replayable`
 * refuses to let it reach for them, and the answer is that they are
 * questions too — asked of the runtime, remembered in the same
 * journal, and therefore replayed rather than re-read.
 *
 * The last two tests are the ones the design exists for: the TAG on a
 * journal entry is what makes a program changeable, because a `Patch`
 * that was not in the old journal must answer `false` WITHOUT eating
 * the answer that follows it.
 */
class TestWf extends munit.FunSuite {

  type P = okay.Pure
  type Row = Delim + P

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  /** a booking that asks the world for a city and the runtime for the time */
  def booking(using Wf.Asking[String, String, String, P]): String ! Row =
    Wf.pause[String, String, String, P]("city?").flatMap: city =>
      Wf.now[String, String, String, P].flatMap: t =>
        Wf.uuid[String, String, String, P].map: id =>
          s"$city/$t/$id"

  test("the runtime answers its own questions; the oracle answers the author's") {
    var asked = List.empty[String]
    val start = !.run(Wf.resumable[String, String, String, P](booking))
    val (r, j) = !.run(Wf.drive(start) { q => asked = asked :+ q; okay.pure("Kyiv") })

    assertEquals(r, "Kyiv/1700000000000/id-1")
    assertEquals(asked, List("city?"), "the oracle was asked the runtime's questions too")
    // the journal remembers BOTH kinds, tagged
    assertEquals(j, List(Right("Kyiv"), Left(Wf.SysA.Millis(1_700_000_000_000L)),
      Left(Wf.SysA.Text("id-1"))))
  }

  test("replay gives the same values — the clock is read from the journal, not the wall") {
    val (r1, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](booking)))(_ => okay.pure("Kyiv")))

    // `Wf.replay` TAKES NO RUNTIME — its signature is the proof that
    // it cannot read a clock: the journal is the only source it has
    val back = !.run(Wf.replay[String, String, String, P](booking)(j))
    assertEquals(back.finished, Some(r1), "replay asked the runtime again")
  }

  test("a die and a clock are each read ONCE, however often the program is replayed") {
    var reads = 0
    given counting: Wf.Runtime = new Wf.Runtime:
      def answer(q: Wf.Sys): Wf.SysA =
        reads += 1
        q match
          case Wf.Sys.Random => Wf.SysA.Dice(0.5)
          case Wf.Sys.Now => Wf.SysA.Millis(7L)
          case Wf.Sys.Uuid => Wf.SysA.Text("x")
          case Wf.Sys.Patch(_) => Wf.SysA.Flag(true)

    def dicey(using Wf.Asking[String, Double, Double, P]): Double ! Row =
      Wf.random[String, Double, Double, P]

    val (v, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, Double, Double, P](dicey)))(_ => okay.pure(0.0)))
    assertEquals(reads, 1)
    val again = !.run(Wf.replay[String, Double, Double, P](dicey)(j))
    assertEquals(again.finished, Some(v))
    assertEquals(reads, 1, "replay rolled the die again")
  }

  // ---- the tag, and what it is for

  /** v1: ask the city, then the nights */
  def v1(using Wf.Asking[String, String, String, P]): String ! Row =
    Wf.pause[String, String, String, P]("city?").flatMap: city =>
      Wf.pause[String, String, String, P]("nights?").map(n => s"$city/$n")

  /** v2: the same, with a branch added BETWEEN the two questions */
  def v2(using Wf.Asking[String, String, String, P]): String ! Row =
    Wf.pause[String, String, String, P]("city?").flatMap: city =>
      Wf.patch[String, String, String, P]("promo").flatMap: on =>
        Wf.pause[String, String, String, P]("nights?").map: n =>
          if on then s"$city/$n/promo" else s"$city/$n"

  test("patch: a journal written BEFORE the branch existed takes the old path") {
    // the old run, under v1
    val (old, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](v1)))(q =>
        okay.pure(if q == "city?" then "Kyiv" else "3")))
    assertEquals(old, "Kyiv/3")
    assertEquals(j, List(Right("Kyiv"), Right("3")))

    // the deploy happens; the SAME journal is now read by v2
    val back = !.run(Wf.replay[String, String, String, P](v2)(j))
    assertEquals(back.finished, Some("Kyiv/3"),
      "the patch ate the answer that followed it, or took the new branch")
  }

  test("patch: a run that STARTS under v2 takes the new path, and it is journalled") {
    val (fresh, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](v2)))(q =>
        okay.pure(if q == "city?" then "Lviv" else "2")))
    assertEquals(fresh, "Lviv/2/promo")
    // the decision is IN the journal, between the two answers
    assertEquals(j, List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("2")))

    // so a third process agrees, and it cannot have asked the runtime
    // because `replay` has nowhere to take one
    assertEquals(!.run(Wf.replay[String, String, String, P](v2)(j)).finished, Some(fresh))
  }

  test("patch: the old journal replays under v2 and can still be FINISHED live") {
    // half an old journal: only the city was answered
    val half = List(Right("Kyiv"))
    val at = !.run(Wf.replay[String, String, String, P](v2)(half))
    // it stands at the patch, which is live now, so the driver decides
    val (r, more) = !.run(Wf.drive(at)(_ => okay.pure("4")))
    assertEquals(r, "Kyiv/4/promo")
    assertEquals(more, List(Left(Wf.SysA.Flag(true)), Right("4")))
  }
}
