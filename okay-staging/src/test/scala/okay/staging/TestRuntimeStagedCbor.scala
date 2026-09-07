package okay.staging

import okay.codec.{Cbor, Codecs, Schema, Staging}

/**
 * The CBOR emitter over a schema value agrees with the interpreter —
 * item for item, Left for Left — and the seam is reached: `install()`
 * makes every `Codecs` door the staged codec, `Staging.autoInstall()`
 * finds this module by name (staging-seam).
 */
class TestRuntimeStagedCbor extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(300, "s")

  final case class Address(street: String, zip: Option[String], floor: Int = 1) derives Schema
  final case class Order(id: Long, user: String, amount: Double, active: Boolean,
                         tags: List[String], addr: Address, note: Option[String],
                         scores: Vector[Double] = Vector.empty) derives Schema
  enum Pet derives Schema:
    case Dog(name: String, age: Int)
    case Cat(name: String)
    case Rock
  final case class Owner(name: String, pet: Pet, pets: List[Pet]) derives Schema
  final case class Port(n: Int)
  given Schema[Port] = Schema.refine((n: Int) => if n >= 1 && n <= 65535 then Right(Port(n)) else Left(s"port $n is out of range"), _.n)
  final case class Server(host: String, port: Port, owner: Option[Long] = None) derives Schema
  final case class Tree(value: Int, kids: List[Tree]) derives Schema
  final case class Raw(name: String, bytes: Array[Byte], c: Char) derives Schema

  private val order = Order(42L, "ada", 12.5, true, List("new", "vip"), Address("Main 1", Some("12345"), 3), None, Vector(1.5, 2.0))
  private val owner = Owner("bo", Pet.Dog("rex", 3), List(Pet.Cat("tom"), Pet.Rock, Pet.Dog("ace", 1)))

  private def agree[A](s: Schema[A], values: Seq[A], damaged: Seq[Array[Byte]]): Unit =
    val staged = RuntimeStaged.cbor(s)
    assert(RuntimeStaged.isStagedCbor(s), s"not staged: ${RuntimeStaged.lastFailure.map(_._2.toString)}")
    for a <- values do
      assert(java.util.Arrays.equals(staged.encode(a), Cbor.write(a)(using s)), s"encode of $a")
      assertEquals(staged.decode(staged.encode(a)), Right(a))
    for bs <- damaged do
      assertEquals(staged.decode(bs), Cbor.read[A](bs)(using s), s"decode of ${bs.toSeq}")

  test("products, nesting, Option, List, Vector, defaults, an absent field, damage") {
    val s = summon[Schema[Order]]
    // an Order written without `scores` (its default), and without `note`
    val partial = Cbor.write(Map("id" -> 1L))(using Schema.SProduct[Map[String, Long]]("Order",
      Vector("id" -> (() => Schema.SLong)), xs => Map("id" -> xs(0).asInstanceOf[Long]), m => Seq(m("id"))))
    agree(s, Seq(order, order.copy(note = Some("x"), tags = Nil, scores = Vector.empty)),
      Seq(partial, Array[Byte](0x80.toByte), Array[Byte](0xa1.toByte, 0x62, 'i', 'd', 0x61, 'x'), Cbor.write(order)(using s).take(9)))
  }

  test("sums: every case, a sum inside a list, an unknown case, the wrong shape") {
    val s = summon[Schema[Owner]]
    val unknown = Cbor.write(Map("Fish" -> 1L))(using Schema.SProduct[Map[String, Long]]("Owner",
      Vector("Fish" -> (() => Schema.SLong)), xs => Map("Fish" -> xs(0).asInstanceOf[Long]), m => Seq(m("Fish"))))
    val pet = summon[Schema[Pet]]
    agree(pet, Seq(Pet.Rock, Pet.Cat("c"), Pet.Dog("d", 2)), Seq(unknown, Array[Byte](0xa2.toByte, 0x61, 'a', 0x01, 0x61, 'b', 0x02), Array[Byte](0x01)))
    agree(s, Seq(owner, owner.copy(pet = Pet.Rock, pets = Nil)), Seq(Array[Byte](0x80.toByte)))
  }

  test("iso, recursion, and the nodes the generator leaves to the fold (bytes, char)") {
    agree(summon[Schema[Server]], Seq(Server("h", Port(80), Some(1L)), Server("h", Port(1))), Seq(Array[Byte](0xa1.toByte, 0x64, 'p', 'o', 'r', 't', 0x19, 0xff.toByte, 0xff.toByte)))
    agree(summon[Schema[Tree]], Seq(Tree(1, List(Tree(2, Nil), Tree(3, List(Tree(4, Nil))))), Tree(0, Nil)), Nil)
    val raw = summon[Schema[Raw]]
    val staged = RuntimeStaged.cbor(raw)
    val r = Raw("n", Array[Byte](1, 2, 3), 'z')
    assert(java.util.Arrays.equals(staged.encode(r), Cbor.write(r)(using raw)))
    val back = staged.decode(staged.encode(r)).toOption.get
    assertEquals((back.name, back.bytes.toSeq, back.c), ("n", Seq[Byte](1, 2, 3), 'z'))
  }

  test("install: every Codecs door answers the staged codec; autoInstall finds the module by name; off is off") {
    Codecs.reset()
    try
      assert(RuntimeStaged.install())
      assertEquals(Codecs.provider.name, "runtime-staged")
      val s = summon[Schema[Server]]
      val v = Server("db", Port(5432))
      assertEquals(Codecs.readJson[Server](Codecs.writeJson(v)), Right(v))
      assert(RuntimeStaged.isStaged(s), "the JSON door went through the generator")
      assertEquals(Codecs.readCbor[Server](Codecs.writeCbor(v)), Right(v))
      assert(RuntimeStaged.isStagedCbor(s), "the CBOR door went through the generator")
      Codecs.reset()
      assertEquals(Staging.autoInstall(), Staging.Outcome.Installed)
      assertEquals(Codecs.provider.name, "runtime-staged")
      Codecs.reset()
      RuntimeStaged.force(Some(false))
      assert(!RuntimeStaged.install())
      assertEquals(Codecs.provider.name, "interpreter")
      assertEquals(Staging.autoInstall(), Staging.Outcome.Refused("switched off (okay.staging=off)"))
    finally
      RuntimeStaged.force(None)
      Codecs.reset()
  }
}
