package okay.ui



/**
 * Keyed children: a moved child is a move, not a Replace — and the
 * law (diff then patch equals the next tree) holds across every
 * combination of shuffle, removal, insertion and edit a seeded
 * generator throws at it.
 */
class TestKeyedDiff extends munit.FunSuite {

  import Ui.*

  def item(k: String, text: String): Ui =
    Column(Vector(Text(text), Button("open", s"$k-open")), key = k)

  def list(items: (String, String)*): Ui =
    Column(items.toVector.map(item), key = "list")

  def apply(from: Ui, patches: Vector[Patch]): Ui = patches.foldLeft(from)(Ui.patch)

  test("a shuffle is ONE Reorder plus nothing — no child Replaced for moving") {
    val a = list("a" -> "alpha", "b" -> "beta", "c" -> "gamma")
    val b = list("c" -> "gamma", "a" -> "alpha", "b" -> "beta")
    val ps = Ui.diff(a, b)
    assertEquals(ps, Vector(Patch.Reorder(Nil, Vector(2, 0, 1))))
    assertEquals(apply(a, ps), b)
  }

  test("a moved AND edited child moves and gets a narrow patch") {
    val a = list("a" -> "alpha", "b" -> "beta")
    val b = list("b" -> "BETA", "a" -> "alpha")
    val ps = Ui.diff(a, b)
    assert(ps.contains(Patch.Reorder(Nil, Vector(1, 0))), ps.toString)
    assert(ps.exists { case Patch.SetText(List(0, 0), "BETA") => true; case _ => false },
      ps.toString)
    assert(!ps.exists(_.isInstanceOf[Patch.Replace]), s"a move became a Replace: $ps")
    assertEquals(apply(a, ps), b)
  }

  test("vanished keys Remove, appeared keys Insert — and the law holds") {
    val a = list("a" -> "alpha", "b" -> "beta", "c" -> "gamma")
    val b = list("c" -> "gamma", "d" -> "delta", "a" -> "alpha")
    val ps = Ui.diff(a, b)
    assert(ps.exists(_.isInstanceOf[Patch.Remove]), ps.toString)
    assert(ps.exists(_.isInstanceOf[Patch.Insert]), ps.toString)
    assertEquals(apply(a, ps), b)
  }

  test("mixed or unkeyed children fall back to the positional walk") {
    // a Text child has no key: the container is NOT keyed-matched
    val a = Column(Vector(Text("head"), item("a", "alpha")), key = "list")
    val b = Column(Vector(Text("head"), item("a", "ALPHA")), key = "list")
    assertEquals(Ui.diff(a, b),
      Vector(Patch.SetText(List(1, 0), "ALPHA")))
    // and different unkeyed lengths still replace the container
    val c = Column(Vector(Text("one")))
    val d = Column(Vector(Text("one"), Text("two")))
    assertEquals(Ui.diff(c, d), Vector(Patch.Replace(Nil, d)))
  }

  test("the law, generated: shuffles, removals, insertions, edits combined") {
    val rnd = scala.util.Random(20260901)
    val pool = Vector("a", "b", "c", "d", "e", "f", "g")
    def tree(keys: Vector[String], salt: Int): Ui =
      Column(keys.map(k => item(k, s"$k-$salt")), key = "list")

    for round <- 1 to 200 do
      val oldKeys = rnd.shuffle(pool).take(1 + rnd.nextInt(pool.length))
      // next: survivors shuffled, some dropped, some added, contents salted
      val survivors = rnd.shuffle(oldKeys.filter(_ => rnd.nextBoolean() || oldKeys.length == 1))
      val added = rnd.shuffle(pool.filterNot(oldKeys.contains)).take(rnd.nextInt(3))
      val newKeys = rnd.shuffle(survivors ++ added)
      val a = tree(oldKeys, salt = 0)
      val b = tree(newKeys, salt = rnd.nextInt(2))
      val ps = Ui.diff(a, b)
      assertEquals(apply(a, ps), b,
        s"round $round: $oldKeys -> $newKeys broke the law with $ps")
      // and quality: a pure shuffle of survivors never Replaces a child
      if newKeys.toSet == oldKeys.toSet then
        assert(!ps.exists { case Patch.Replace(p, _) => p.nonEmpty; case _ => false },
          s"round $round: a moved child was Replaced: $ps")

    // nested keyed lists keep the law too
    val n1 = Column(Vector(list("a" -> "x", "b" -> "y"), list("c" -> "z")), key = "outer")
    val n2 = Column(Vector(list("c" -> "z"), list("b" -> "y", "a" -> "X")), key = "outer")
    // outer children share the key "list" — NOT distinct, so the
    // outer level is positional; the inner keyed levels still move
    assertEquals(apply(n1, Ui.diff(n1, n2)), n2)
  }
}
