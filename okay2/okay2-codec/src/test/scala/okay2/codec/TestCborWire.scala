package okay2.codec

/** The CBOR halves of okay-codec's TestCodec, TestDefaults, TestVector,
 * TestIso, TestEnumeration and TestIntRange (their JSON halves are the
 * okay2 suites of the same names): the same Schema on the second wire,
 * decoding to the value JSON decodes to. */
class TestCborWire extends munit.FunSuite {

  test("round-trip: a nested, recursive product, and the same answer as JSON") {
    val p = Person("ann", 41, List("a", "b"), Some(Person("bo\"ss", 60, Nil, None)))
    assertEquals(Cbor.read[Person](Cbor.write(p)), Right(p))
    assertEquals(Cbor.read[Person](Cbor.write(p)), Json.read[Person](Json.write(p)))
  }

  test("round-trip: sums by case name, a case object included") {
    val shapes: List[Shape] = List(Shape.Circle(1.5), Shape.Rect(2, 3), Shape.Dot)
    for (s <- shapes) {
      assertEquals(Cbor.read[Shape](Cbor.write(s)), Right(s))
      assertEquals(Cbor.read[Shape](Cbor.write(s)), Json.read[Shape](Json.write(s)))
    }
  }

  test("decode errors are values: truncated, wrong shape, empty") {
    val bytes = Cbor.write(Person("x", 1, Nil, None))
    assert(Cbor.read[Person](bytes.dropRight(3)).isLeft)
    assert(Cbor.read[Shape](bytes).isLeft)
    assert(Cbor.read[Person](Array[Byte]()).isLeft)
  }

  test("bytes travel as a CBOR byte string: the payload plus a few bytes of head") {
    final case class Blob(name: String, data: Array[Byte])
    implicit val blob: Schema[Blob] = Schema.derived
    val packed = Array.tabulate(1024)(i => (i * 31).toByte)
    val cbor = Cbor.write(Blob("v", packed))
    // a map head, "name" "v", "data", a 3-byte byte-string head, the payload
    assertEquals(cbor.length, 1 + 5 + 2 + 5 + 3 + 1024, "no per-byte cost")
    assertEquals(Cbor.read[Blob](cbor).map(_.data.toList), Right(packed.toList))
  }

  test("defaults: absent defaulted fields take their declarations; the full wire decodes exactly") {
    final case class PartialJob(name: String)
    implicit val partialJob: Schema[PartialJob] = Schema.derived
    assert(Cbor.read[Strict](Cbor.write(Strict("a", 1))).isRight)
    assertEquals(Cbor.read[Job](Cbor.write(PartialJob("x"))), Right(Job("x", 3, Some(5), false)))
    val job = Job("y", 1, None, true)
    assertEquals(Cbor.read[Job](Cbor.write(job)), Right(job))
  }

  test("Vector round-trips, nested included; a recursive type too") {
    val b = Bag(Vector("a", "b c"), Vector(1, -2, 3), Vector(Vector(true), Vector.empty, Vector(false, true)))
    assertEquals(Cbor.read[Bag](Cbor.write(b)), Right(b))
    def deep(n: Int): Tree =
      if (n == 0) Tree("leaf", Vector.empty)
      else Tree(s"n$n", Vector(deep(n - 1), Tree(s"s$n", Vector.empty)))
    val t = deep(50)
    assertEquals(Cbor.read[Tree](Cbor.write(t)), Right(t))
  }

  test("a wrapper travels bare, and a refined one still refuses") {
    assertEquals(Cbor.read[UserId](Cbor.write(UserId(7))), Right(UserId(7)))
    assertEquals(Cbor.write(UserId(7)).toList, Cbor.write(7L).toList)
    val s = Server("db1", Port(5432), Some(UserId(9)))
    assertEquals(Cbor.read[Server](Cbor.write(s)), Right(s))
    assertEquals(Cbor.read[Port](Cbor.write(70000)), Left("port 70000 is out of range"))
  }

  test("an enumeration travels by name") {
    assertEquals(Cbor.read[Hue](Cbor.write[Hue](Hue.Red)), Right(Hue.Red))
    assertEquals(Cbor.read[String](Cbor.write[Hue](Hue.Red)), Right("red"))
  }

  test("an Int field refuses a CBOR integer past Int, and reads its extremes") {
    assert(Cbor.read[IntBox](Cbor.write(LongBox(1L << 32))).isLeft)
    assertEquals(Cbor.read[IntBox](Cbor.write(IntBox(Int.MinValue))), Right(IntBox(Int.MinValue)))
    assertEquals(Cbor.read[IntBox](Cbor.write(IntBox(Int.MaxValue))), Right(IntBox(Int.MaxValue)))
  }
}
