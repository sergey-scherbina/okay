package okay

/**
 * specs/strymonas-zip-fusion.md: `Gen.zip`, and strymonas's hard case
 * (Kiselyov, Biboudis, Palladinos & Smaragdakis, "Stream fusion, to
 * completeness", POPL 2017) — zipping a generator built by `flatMap`
 * without materializing it first. Laziness proofs use TestGen's own
 * `Counted` witness shape: a body that counts its own steps.
 */
class TestGenZip extends munit.FunSuite:

  class Counted:
    var steps = 0
    def gen(from: Int): Gen[Int] =
      def go(i: Int): Unit ! Gen.Row[Int] =
        steps += 1
        Gen.emit(i).program.flatMap(_ => go(i + 1))
      Gen.fromProgram(Free.delay(() => go(from)))

  test("zip: two equal-length sources pair up in order") {
    assertEquals(Gen(1, 2, 3).zip(Gen("a", "b", "c")).toList, List((1, "a"), (2, "b"), (3, "c")))
  }

  test("zip: stops at the SHORTER side, either side") {
    assertEquals(Gen(1, 2, 3, 4, 5).zip(Gen("a", "b")).toList, List((1, "a"), (2, "b")))
    assertEquals(Gen(1, 2).zip(Gen("a", "b", "c", "d")).toList, List((1, "a"), (2, "b")))
  }

  test("zip: either side empty gives an empty zip") {
    assertEquals(Gen.empty[Int].zip(Gen(1, 2, 3)).toList, Nil)
    assertEquals(Gen(1, 2, 3).zip(Gen.empty[String]).toList, Nil)
  }

  test("zipWith: the pair combined in the same pass") {
    assertEquals(Gen(1, 2, 3).zipWith(Gen(10, 20, 30))(_ + _).toList, List(11, 22, 33))
  }

  test("zip composes with a further Xf stage — the usual fusion after it") {
    assertEquals(Gen(1, 2, 3, 4).zip(Gen("a", "b", "c")).map(_._1 * 10).filter(_ > 10).toList,
      List(20, 30))
  }

  test("zip after ++ (Cat) on one side") {
    val left = Gen(1, 2) ++ Gen(3, 4, 5)
    assertEquals(left.zip(Gen("a", "b", "c", "d")).toList, List((1, "a"), (2, "b"), (3, "c"), (4, "d")))
  }

  test("guide: the pinned flatMap-zip example, verbatim") {
    val left = Gen(1, 2, 3, 4).flatMap(i => Gen.from(Vector.fill(i)(i)))   // 1, 2,2, 3,3,3, 4,4,4,4
    val zipped = left.zip(Gen('a', 'b', 'c', 'd', 'e', 'f')).toList
    // zipped == List(1 -> 'a', 2 -> 'b', 2 -> 'c', 3 -> 'd', 3 -> 'e', 3 -> 'f')
    assertEquals(zipped, List(1 -> 'a', 2 -> 'b', 2 -> 'c', 3 -> 'd', 3 -> 'e', 3 -> 'f'))
  }

  // ---------------------------------------------------------- the hard case

  test("STRYMONAS'S HARD CASE: zip a flatMap-fused generator, without materializing it first") {
    // the LEFT side is a flatMap: each outer i yields i copies of i —
    // 1, 2,2, 3,3,3, ... — the exact "several yields per outer step"
    // shape flatMap fusion exists for; zip pulls through it one
    // element at a time via the SAME `.program` every other reader uses
    val left: Gen[Int] = Gen(1, 2, 3, 4).flatMap(i => Gen.from(Vector.fill(i)(i)))
    val right: Gen[Char] = Gen('a', 'b', 'c', 'd', 'e', 'f')
    // left: 1, 2, 2, 3, 3, 3, 4, 4, 4, 4 (10 elements); right has 6
    assertEquals(left.zip(right).toList,
      List(1 -> 'a', 2 -> 'b', 2 -> 'c', 3 -> 'd', 3 -> 'e', 3 -> 'f'))
  }

  test("the hard case is LAZY: zip through a flatMap-fused side stops the OUTER source too") {
    val outer = Counted()
    // the flatMap's inner Gen is plain (Gen.from), so the witness on
    // the OUTER source shows the whole chain — outer AND inner — ran
    // no further than zip actually needed
    val left: Gen[Int] = outer.gen(1).flatMap(i => Gen.from(Vector.fill(2)(i)))
    // left, unfused, would be 1,1, 2,2, 3,3, ...; the 3rd zipped
    // pair needs the 2nd outer step's SECOND copy, so exactly steps
    // 1 and 2 run — verified by running, not assumed (measured: 2)
    val z = left.zip(Gen("x", "y", "z"))
    assertEquals(z.toList, List(1 -> "x", 1 -> "y", 2 -> "z"))
    assertEquals(outer.steps, 2, "3 zipped elements need only the 1st and 2nd outer steps (each yields 2 inner copies) — not the 3rd or 4th")
  }

  test("the hard case, laziness against an INFINITE outer source — the strongest witness") {
    // an infinite flatMap-fused generator: unfused, or read eagerly,
    // this would never finish; zip through it, stopped by a finite
    // right side, must terminate
    val infiniteDoubled: Gen[Int] = Gen.unfold(1)(n => Some((n, n + 1))).flatMap(i => Gen.from(Vector(i, i)))
    assertEquals(infiniteDoubled.zip(Gen("p", "q", "r", "s", "t")).toList,
      List(1 -> "p", 1 -> "q", 2 -> "r", 2 -> "s", 3 -> "t"))
  }

  test("zip is symmetric in what it accepts: the RIGHT side may be the flatMap-fused one") {
    val right: Gen[Int] = Gen(1, 2, 3).flatMap(i => Gen.from(Vector.fill(i)(i)))
    assertEquals(Gen("a", "b", "c", "d", "e", "f").zip(right).toList,
      List("a" -> 1, "b" -> 2, "c" -> 2, "d" -> 3, "e" -> 3, "f" -> 3))
  }
