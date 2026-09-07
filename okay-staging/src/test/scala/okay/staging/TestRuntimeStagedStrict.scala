package okay.staging

import okay.codec.{Codecs, Json, JsonStrict, Schema}

/**
 * The strict reader over a schema VALUE agrees with `JsonStrict.read`
 * — the same value on a well-formed document, the same Left on every
 * refusal (staged-strict). And it reaches the seam: `Codecs.readStrict`
 * answers the generated reader once okay-staging is installed, the
 * interpreted walk before that.
 */
class TestRuntimeStagedStrict extends munit.FunSuite {

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

  private val order = Order(42L, "ada \"the\" first", 12.5, true, List("new", "vip"),
    Address("Main 1", Some("12345"), 3), None, Vector(1.5, 2.0))
  private val owner = Owner("bo", Pet.Dog("rex", 3), List(Pet.Cat("tom"), Pet.Rock, Pet.Dog("ace", 1)))

  /** every text answers exactly what the interpreted strict read answers */
  private def agree[A](s: Schema[A], values: Seq[A], texts: Seq[String]): Unit =
    val staged = RuntimeStaged.strict(s)
    assert(RuntimeStaged.isStagedStrict(s), s"not staged: ${RuntimeStaged.lastFailure.map(_._2.toString)}")
    for a <- values do
      val text = Json.encode(s)(a)
      assertEquals(staged.decode(text), Right(a), s"round trip of $a")
      assertEquals(staged.decode(text), JsonStrict.read[A](text)(using s))
    for t <- texts do
      assertEquals(staged.decode(t), JsonStrict.read[A](t)(using s), s"on $t")

  test("products, nesting, Option, List, Vector, defaults, and every refusal the fold makes") {
    val s = summon[Schema[Order]]
    agree(s, Seq(order, order.copy(note = Some("x"), tags = Nil, scores = Vector.empty)),
      Seq("""{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s","zip":null},"note":null}""",
          """{"id":1,"user":"u","amount":2.5,"active":false,"tags":["a"],"addr":{"street":"s"},"note":"n","scores":[1]}""",
          // an unknown field is skipped, as the fold skips it
          """{"extra":[1,{"deep":true}],"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s"}}""",
          """{"id":"one","user":"u"}""",                    // wrong primitive
          """{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s"}""",  // truncated
          """{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{}}""",             // missing required
          """[1,2]""",                                       // wrong shape
          """{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s"}} tail"""))
  }

  test("sums: every case, an unknown case, and a one-entry object that is not one") {
    agree(summon[Schema[Owner]], Seq(owner, owner.copy(pet = Pet.Rock, pets = Nil)),
      Seq("""{"name":"b","pet":{"Cat":{"name":"c"}},"pets":[]}""",
          """{"name":"b","pet":{"Fish":{"name":"c"}},"pets":[]}""",
          """{"name":"b","pet":{"Cat":{"name":"c"},"Dog":{"name":"d","age":1}},"pets":[]}""",
          """{"name":"b","pet":"Rock","pets":[]}"""))
  }

  test("iso, recursion, and the nodes it leaves to the fold (bytes, char)") {
    agree(summon[Schema[Server]], Seq(Server("h", Port(80), Some(1L)), Server("h", Port(443))),
      Seq("""{"host":"h","port":70000}""", """{"host":"h","port":"80"}"""))
    agree(summon[Schema[Tree]], Seq(Tree(1, List(Tree(2, Nil), Tree(3, List(Tree(4, Nil))))), Tree(0, Nil)),
      Seq("""{"value":1,"kids":[{"value":2}]}""", """{"value":1,"kids":{}}"""))
    val raw = summon[Schema[Raw]]
    val staged = RuntimeStaged.strict(raw)
    val text = Json.encode(raw)(Raw("n", Array[Byte](1, 2, 3), 'z'))
    val back = staged.decode(text).toOption.get
    assertEquals((back.name, back.bytes.toSeq, back.c), ("n", Seq[Byte](1, 2, 3), 'z'))
    assertEquals(staged.decode("""{"name":"n","bytes":"!!","c":"zz"}""").isLeft,
      JsonStrict.read[Raw]("""{"name":"n","bytes":"!!","c":"zz"}""")(using raw).isLeft)
  }

  test("the seam: Codecs.readStrict is the fold until installed, the generated reader after") {
    Codecs.reset()
    val s = summon[Schema[Server]]
    val text = Json.encode(s)(Server("db", Port(5432)))
    try
      assertEquals(Codecs.readStrict[Server](text), Right(Server("db", Port(5432))))
      assertEquals(Codecs.provider.name, "interpreter")
      assert(RuntimeStaged.install())
      assertEquals(Codecs.readStrict[Server](text), Right(Server("db", Port(5432))))
      assert(RuntimeStaged.isStagedStrict(s), "the strict door went through the generator")
      RuntimeStaged.force(Some(false))
      assertEquals(RuntimeStaged.strict(s).decode(text), Right(Server("db", Port(5432))))
    finally
      RuntimeStaged.force(None)
      Codecs.reset()
  }
}
