package okay2.codec

final case class Owner(name: String, pet: Pet, pets: List[Pet], nick: Option[String])

/** A schema's edges are lazy AND memoised: a thunk answers the same
 * instance every time, and recursion still terminates because nothing
 * is forced at construction (okay-codec's TestSchemaOnce). */
class TestSchemaOnce extends munit.FunSuite {

  private val port: Schema[Port] = Port.schema

  private def thunkOf(s: Schema[_], i: Int): () => Schema[_] = s match {
    case p: Schema.SProduct[_] => p.fields(i)._2
    case su: Schema.SSum[_] => su.cases(i)._2
    case Schema.SOption(of) => of
    case Schema.SList(of) => of
    case Schema.SVector(of) => of
    case iso: Schema.SIso[_, _] => iso.under
    case other => fail(s"no thunk on $other")
  }

  test("a sum's case thunk (a subtype with no instance of its own) answers one instance") {
    val pet = implicitly[Schema[Pet]]
    val dog = thunkOf(pet, 0)
    assert(dog() eq dog(), "Dog's schema was re-derived per call")
    assert(thunkOf(pet, 1)() eq thunkOf(pet, 1)())
  }

  test("a product's field thunks, Option/List edges and an iso's under answer one instance") {
    val owner = implicitly[Schema[Owner]]
    for (i <- 0 until 4) {
      val t = thunkOf(owner, i)
      assert(t() eq t(), s"field $i re-derived")
    }
    val pets = thunkOf(owner, 2)()
    assert(thunkOf(pets, 0)() eq thunkOf(pets, 0)(), "List's element thunk")
    val nick = thunkOf(owner, 3)()
    assert(thunkOf(nick, 0)() eq thunkOf(nick, 0)(), "Option's element thunk")
    assert(thunkOf(port, 0)() eq thunkOf(port, 0)(), "iso's under thunk")
  }

  test("recursion terminates, and the recursive edge is the declared instance itself") {
    val tree = implicitly[Schema[Tree]]
    val kids = thunkOf(tree, 1)()
    val again = thunkOf(kids, 0)()
    assert(again eq tree, "the recursive edge is the same instance, not a copy")
    val t = Tree("1", Vector(Tree("2", Vector.empty)))
    assertEquals(Json.decode(tree)(Json.parse(Json.encode(tree)(t))), Right(t))
  }

  test("nothing is forced at construction") {
    var forced = 0
    val lazyEdge = Schema.once[Int] { forced += 1; Schema.SInt }
    assertEquals(forced, 0)
    assert(lazyEdge() eq lazyEdge())
    assertEquals(forced, 1)
  }
}
