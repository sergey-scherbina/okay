package okay.staging

import okay.codec.{Json, Schema}

/**
 * The run-time staged codec agrees with the interpreter — encode byte
 * for byte, decode Left for Left — over the whole node vocabulary, on
 * schemas the generator only ever sees as VALUES (specs/codecs.md,
 * staged-runtime). And the door is optional: off, it is the
 * interpreter and no compiler is made.
 */
class TestRuntimeStaged extends munit.FunSuite {

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
  final case class UserId(n: Long)
  given Schema[UserId] = Schema.wrap(UserId(_), _.n)
  final case class Port(n: Int)
  given Schema[Port] = Schema.refine((n: Int) => if n >= 1 && n <= 65535 then Right(Port(n)) else Left(s"port $n is out of range"), _.n)
  enum Colour:
    case Red, Green
  given Schema[Colour] = Schema.enumeration[Colour, String](Colour.values.toVector, _.toString.toLowerCase)
  final case class Server(host: String, port: Port, owner: Option[UserId] = None, colour: Colour = Colour.Red) derives Schema
  final case class Tree(value: Int, kids: List[Tree]) derives Schema

  private val order = Order(42L, "ada \"the\" first", 12.5, true, List("new", "vip"), Address("Main 1", Some("12345"), 3), None, Vector(1.5, 2.0))
  private val owner = Owner("bo", Pet.Dog("rex", 3), List(Pet.Cat("tom"), Pet.Rock, Pet.Dog("ace", 1)))
  private val server = Server("db1", Port(5432), Some(UserId(9)), Colour.Green)
  private val tree = Tree(1, List(Tree(2, Nil), Tree(3, List(Tree(4, Nil)))))

  private def agree[A](s: Schema[A], values: Seq[A], texts: Seq[String]): Unit =
    val staged = RuntimeStaged.json(s)
    assert(RuntimeStaged.isStaged(s), s"not staged: ${RuntimeStaged.lastFailure.map(_._2.toString)}")
    for a <- values do
      assertEquals(staged.encode(a), Json.encode(s)(a), s"encode of $a")
      assertEquals(staged.decode(Json.parse(staged.encode(a))), Right(a))
    for t <- texts do
      assertEquals(staged.decode(Json.parse(t)), Json.decode(s)(Json.parse(t)), s"decode of $t")

  test("products, nesting, Option, List, Vector, defaults — encode byte for byte, decode Left for Left") {
    val s = summon[Schema[Order]]
    agree(s, Seq(order, order.copy(note = Some("x"), tags = Nil, scores = Vector.empty)),
      Seq("""{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s","zip":null},"note":null}""",
          """{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s"},"note":"n","scores":[1]}""",
          """{"id":"one","user":"u"}""", """[1,2]""", """{"id":1,"user":"u","amount":2.5,"active":false,"tags":["a",3],"addr":{"street":"s"}}""",
          """{"id":1,"user":"u","amount":2.5,"active":false,"tags":[],"addr":{"street":"s","floor":"top"},"note":null}"""))
  }

  test("sums: every case, a sum inside a list, an unknown case, the wrong shape") {
    val s = summon[Schema[Owner]]
    agree(s, Seq(owner, owner.copy(pet = Pet.Rock, pets = Nil)),
      Seq("""{"name":"bo","pet":{"Cat":{"name":"c"}},"pets":[]}""", """{"name":"bo","pet":{"Fish":{"name":"c"}},"pets":[]}""",
          """{"name":"bo","pet":{"Dog":{"name":"c"}},"pets":[]}""", """{"name":"bo","pet":"Rock","pets":[]}""",
          """{"name":"bo","pet":{"Rock":{}},"pets":[{"Cat":{"name":"t"}},{"Dog":{"name":"a","age":1}}]}"""))
  }

  test("iso: a wrap travels bare, a refine's Left is the same Left, an enumeration reads its names") {
    val s = summon[Schema[Server]]
    agree(s, Seq(server, server.copy(owner = None)),
      Seq("""{"host":"h","port":70000}""", """{"host":"h","port":80,"owner":3,"colour":"green"}""",
          """{"host":"h","port":80,"colour":"puce"}""", """{"host":"h","port":80}"""))
  }

  test("recursion delegates to the fold and still agrees") {
    agree(summon[Schema[Tree]], Seq(tree, Tree(0, Nil)), Seq("""{"value":1,"kids":[{"value":2,"kids":[]}]}""", """{"value":1}"""))
  }

  test("a schema BUILT at run time — a composite the catalog would describe — stages like any other") {
    // what okay-sql builds from pg_type: a product over Seq[Any] with no case class behind it
    val point: Schema[Seq[Any]] = Schema.SProduct[Seq[Any]]("point",
      Vector("x" -> (() => Schema.SDouble), "y" -> (() => Schema.SDouble), "label" -> (() => Schema.SOption(() => Schema.SString))),
      parts => parts, a => a)
    val rows: Schema[Vector[Seq[Any]]] = Schema.SVector(() => point)
    val staged = RuntimeStaged.json(rows)
    assert(RuntimeStaged.isStaged(rows), RuntimeStaged.lastFailure.map(_._2.toString).getOrElse(""))
    val vs: Vector[Seq[Any]] = Vector(Seq(1.5, 2.5, Some("a")), Seq(0.0, -1.0, None))
    assertEquals(staged.encode(vs), Json.encode(rows)(vs))
    assertEquals(staged.decode(Json.parse(staged.encode(vs))), Right(vs))
    assertEquals(staged.decode(Json.parse("""[{"x":1,"y":"two"}]""")), Json.decode(rows)(Json.parse("""[{"x":1,"y":"two"}]""")))
  }

  test("the switch: off, the door is the interpreter and stages nothing; on, the same schema is generated once") {
    final case class Small(a: Int, b: String) derives Schema
    val s = summon[Schema[Small]]
    RuntimeStaged.force(Some(false))
    try
      val off = RuntimeStaged.json(s)
      assert(!RuntimeStaged.isStaged(s), "off must not generate")
      assertEquals(off.encode(Small(1, "x")), """{"a":1,"b":"x"}""")
    finally RuntimeStaged.force(None)
    val on = RuntimeStaged.json(s)
    assert(RuntimeStaged.isStaged(s))
    assert(RuntimeStaged.json(s) eq RuntimeStaged.json(s), "cached by identity")
    assertEquals(on.encode(Small(1, "x")), """{"a":1,"b":"x"}""")
    assert(!RuntimeStaged.lastFailure.exists(_._1 eq s), "this schema never fell back")
  }
}
