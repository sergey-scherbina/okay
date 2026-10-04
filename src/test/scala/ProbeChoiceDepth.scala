package okay

/** cont-program-leaf-always, red-first probe: does a deep Choose program nest strict runs on the host stack? */
class ProbeChoiceDepth extends munit.FunSuite:

  private def chain(n: Int): Int ! Choose =
    (1 to n).foldLeft(pure[Choose, Int](0))((m, _) => m.flatMap(x => Choose(Seq(x + 1)).perform))

  test("runChoice over 100 000 one-way choice points: switches made") {
    val before = StackSwitch.switches.get()
    val r = runChoice(chain(100000)).run
    println(s"PROBE choice switches=${StackSwitch.switches.get() - before} room=${StackSwitch.firstRoom} result=${r.take(1)}")
  }

  test("runExact over 100 000 one-way dists: switches made") {
    val before = StackSwitch.switches.get()
    val p = (1 to 100000).foldLeft(pure[Dist, Int](0))((m, _) => m.flatMap(x => Prob.dist(x + 1 -> 1.0)))
    val r = Prob.runExact(p).run
    println(s"PROBE exact switches=${StackSwitch.switches.get() - before} result=${r.size}")
  }
