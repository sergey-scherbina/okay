package okay.ui

import okay.{modify, opticFunction1}

/**
 * ui-path-two-walks: what the affine costs against the hand walk.
 *
 * `Ui.patch`'s private `at` and `Ui.path`'s `childAt` walk ONE index
 * convention in two bodies, held equal by a law rather than by being
 * one function (`count-the-doors`). The proposal was to make `patch`
 * at a path be `Ui.path(p).modify(f)`; the backlog entry says measure
 * first, because this affine is built from a RUNTIME `List[Int]` and
 * so pays the interpreter where an optic named in code is free.
 *
 * Not a test — a timing test would measure the box. Run it:
 *   sbt "okayUiJVM/Test/runMain okay.ui.PathWalkProbe"
 */
object PathWalkProbe:

  private def deepTree(depth: Int): Ui =
    (0 until depth).foldLeft(Ui.Text("leaf"): Ui)((u, i) => Ui.Column(Vector(Ui.Text(s"pad $i"), u), s"c$i"))

  /** the path to the leaf of `deepTree`: child 1 all the way down */
  private def pathTo(depth: Int): List[Int] = List.fill(depth)(1)

  private def alloc: Long =
    java.lang.management.ManagementFactory.getThreadMXBean
      .asInstanceOf[com.sun.management.ThreadMXBean]
      .getCurrentThreadAllocatedBytes

  private def round(name: String, n: Int)(f: () => Unit): (Double, Long) =
    var i = 0
    while i < n / 10 do { f(); i += 1 }          // warm
    val a0 = alloc
    val t0 = System.nanoTime()
    i = 0
    while i < n do { f(); i += 1 }
    val ns = (System.nanoTime() - t0).toDouble / n
    val bytes = (alloc - a0) / n
    println(f"$name%-22s $ns%8.1f ns/op  $bytes%8d B/op")
    (ns, bytes)

  /** does the hand walk survive a path that names nothing? the affine
   * is total by construction; `at` indexes a Vector directly */
  private def totality(): Unit =
    val t = Ui.Column(Vector(Ui.Text("a")), "c")
    for p <- Vector(List(9), List(0, 0), List(1, 2, 3)) do
      val viaOptic = scala.util.Try(Ui.path(p).modify(identity)(t)).toEither.left.map(_.getClass.getSimpleName)
      val viaPatch = scala.util.Try(Ui.patch(t, Patch.SetText(p, "x"))).toEither.left.map(_.getClass.getSimpleName)
      println(s"  path $p -> optic: ${viaOptic.fold(e => "THREW " + e, _ => "total")}, " +
        s"patch: ${viaPatch.fold(e => "THREW " + e, _ => "total")}")

  def main(args: Array[String]): Unit =
    println("-- totality on a path that names nothing")
    totality()
    for depth <- Vector(4, 16, 64) do
      val tree = deepTree(depth)
      val p = pathTo(depth)
      val patch = Patch.SetText(p, "next")
      // the two must AGREE before either is timed, or the numbers
      // compare different answers
      val hand = Ui.patch(tree, patch)
      val affine = Ui.path(p).modify { case Ui.Text(_, st) => Ui.Text("next", st); case u => u }(tree)
      require(hand == affine, s"the two walks disagree at depth $depth")
      println(s"-- depth $depth (they agree)")
      val n = 200000
      val best = Vector.fill(3) {
        val a = round("hand walk (Ui.patch)", n)(() => { val _ = Ui.patch(tree, patch) })
        val b = round("affine (Ui.path)", n)(() =>
          { val _ = Ui.path(p).modify { case Ui.Text(_, st) => Ui.Text("next", st); case u => u }(tree) })
        (a, b)
      }.minBy(_._1._1)
      val ((hns, hb), (ans, ab)) = best
      println(f"   ratio ${ans / hns}%.2fx time, ${if hb == 0 then 0.0 else ab.toDouble / hb}%.2fx bytes%n")
