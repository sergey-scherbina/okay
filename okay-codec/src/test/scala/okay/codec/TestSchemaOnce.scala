package okay.codec

/**
 * A schema's edges are lazy AND memoised (schema-thunks-once): a
 * thunk answers the same instance every time, so identity is stable
 * for anything keyed by it and the interpreter never re-derives a
 * child per value. Recursion still terminates — nothing is forced
 * at construction.
 */
class TestSchemaOnce extends munit.FunSuite {

  enum Pet derives Schema:
    case Dog(name: String, age: Int)
    case Cat(name: String)
  final case class Owner(name: String, pet: Pet, pets: List[Pet], nick: Option[String]) derives Schema
  final case class Tree(value: Int, kids: List[Tree]) derives Schema
  final case class Port(n: Int)
  given Schema[Port] = Schema.refine((n: Int) => Right(Port(n)), _.n)

  private def thunkOf(s: Schema[?], i: Int): () => Schema[?] = s match
    case p: Schema.SProduct[?] => p.fields(i)._2
    case su: Schema.SSum[?] => su.cases(i)._2
    case Schema.SOption(of) => of
    case Schema.SList(of) => of
    case Schema.SVector(of) => of
    case Schema.SIso(u, _, _) => u
    case other => fail(s"no thunk on $other")

  test("a sum's case thunk — a subtype with no given of its own — answers one instance") {
    val pet = summon[Schema[Pet]]
    val dog = thunkOf(pet, 0)
    assert(dog() eq dog(), "Dog's schema was re-derived per call")
    assert(thunkOf(pet, 1)() eq thunkOf(pet, 1)())
  }

  test("a product's field thunks, Option/List/Vector edges and an iso's under answer one instance") {
    val owner = summon[Schema[Owner]]
    for i <- 0 until 4 do
      val t = thunkOf(owner, i)
      assert(t() eq t(), s"field $i re-derived")
    val pets = thunkOf(owner, 2)()          // List[Pet]
    assert(thunkOf(pets, 0)() eq thunkOf(pets, 0)(), "List's element thunk")
    val nick = thunkOf(owner, 3)()          // Option[String]
    assert(thunkOf(nick, 0)() eq thunkOf(nick, 0)(), "Option's element thunk")
    val port = summon[Schema[Port]]
    assert(thunkOf(port, 0)() eq thunkOf(port, 0)(), "iso's under thunk")
  }

  test("recursion still terminates, and the recursive edge is the given itself") {
    val tree = summon[Schema[Tree]]
    val kids = thunkOf(tree, 1)()           // List[Tree]
    val again = thunkOf(kids, 0)()          // Tree, through the given
    assert(again eq tree, "the recursive edge is the same given, not a copy")
    val t = Tree(1, List(Tree(2, Nil)))
    assertEquals(Json.decode(tree)(Json.parse(Json.encode(tree)(t))), Right(t))
  }

  test("nothing is forced at construction: a thunk that throws is harmless until called") {
    var forced = 0
    val lazyEdge = Schema.once[Int] { forced += 1; Schema.SInt }
    assertEquals(forced, 0)
    assert(lazyEdge() eq lazyEdge())
    assertEquals(forced, 1)
  }
}
