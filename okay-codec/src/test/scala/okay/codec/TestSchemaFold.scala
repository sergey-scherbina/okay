package okay.codec

/**
 * specs/schema-fold.md, stage 1: `Schema.fold` is the catamorphism the
 * header promised, and `JsonSchema.of` is its first algebra.
 *
 * Three claims. (1) Byte-for-byte: on every non-recursive schema the
 * fold answers what the hand-rolled `of` answered — proved against a
 * verbatim copy of the old code kept HERE (`legacy`), not against a
 * remembered string. (2) A recursive schema folds to a FINITE value:
 * the old `of` descended for ever on `Tree` (a StackOverflowError,
 * checked below on the legacy copy); the fold ties the knot and the
 * schema says `$defs`/`$ref`. (3) The knot is IDENTITY: a named node
 * is folded once however many edges reach it, and the back edge is
 * handed to the algebra as `ref`, not re-folded.
 */
class TestSchemaFold extends munit.FunSuite:

  // ---- (1) the hand-rolled `of`, verbatim, as it stood before the fold
  private def legacy[A](s: Schema[A], vocabularies: Boolean = true): Json = s match
    case iso @ Schema.SIso(u, _, _) => iso.vocabulary match
      case Some(vs) if vocabularies => legacy(u(), vocabularies) match
        case Json.JObj(fs) => Json.JObj(fs :+ ("enum" -> Json.JArr(vs.map(v => Json.parse(Json.encode(u())(v))))))
        case other => other
      case _ => legacy(u(), vocabularies)
    case Schema.SInt | Schema.SLong => obj("type" -> Json.JStr("integer"))
    case Schema.SDouble => obj("type" -> Json.JStr("number"))
    case Schema.SBool => obj("type" -> Json.JStr("boolean"))
    case Schema.SString => obj("type" -> Json.JStr("string"))
    case Schema.SChar => obj("type" -> Json.JStr("string"),
      "minLength" -> Json.JNum(1), "maxLength" -> Json.JNum(1))
    case Schema.SBytes => obj("type" -> Json.JStr("string"),
      "contentEncoding" -> Json.JStr("base64"))
    case Schema.SOption(inner) => legacy(inner(), vocabularies)
    case Schema.SList(inner) => obj("type" -> Json.JStr("array"), "items" -> legacy(inner(), vocabularies))
    case Schema.SVector(inner) => obj("type" -> Json.JStr("array"), "items" -> legacy(inner(), vocabularies))
    case p: Schema.SProduct[A] =>
      def defaulted(i: Int) = p.defaults.lift(i).flatten
      val props = p.fields.zipWithIndex.map { case ((n, f), i) =>
        val base = legacy(f(), vocabularies)
        (n, defaulted(i) match
          case Some(_) => base match
            case Json.JObj(fs) => Json.JObj(fs :+
              ("default" -> Json.parse(p.defaultAt(i)([X] => (sc: Schema[X], x: X) => Json.encode(sc)(x)).get)))
            case other => other
          case None => base)
      }
      val required = p.fields.zipWithIndex.collect {
        case ((n, f), i) if !f().isInstanceOf[Schema.SOption[?]] && defaulted(i).isEmpty => Json.JStr(n)
      }
      obj("type" -> Json.JStr("object"), "properties" -> Json.JObj(props), "required" -> Json.JArr(required))
    case su: Schema.SSum[A] =>
      obj("oneOf" -> Json.JArr(su.cases.map { (n, c) =>
        obj("type" -> Json.JStr("object"),
          "properties" -> Json.JObj(Vector((n, legacy(c(), vocabularies)))),
          "required" -> Json.JArr(Vector(Json.JStr(n))))
      }))
  private def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)

  enum Colour derives Schema:
    case Red, Green
  final case class Email(value: String)
  given Schema[Email] = Schema.wrap[Email, String](Email(_), _.value)
  enum Level:
    case Low, High
  given Schema[Level] = Schema.enumeration(Vector(Level.Low, Level.High), _.toString.toLowerCase)
  final case class Address(city: String, zip: Int = 0)
  given Schema[Address] = Schema.derived
  final case class Order(id: Int, tags: List[String], colour: Colour, email: Option[Email],
                         amounts: Vector[Double], level: Level, address: Address, raw: Array[Byte], c: Char)
  given Schema[Order] = Schema.derived
  enum Shape derives Schema:
    case Dot
    case Box(w: Int, h: Int, inner: Option[Address])

  test("byte-for-byte with the hand-rolled fold on every non-recursive shape, both vocabulary settings") {
    def check[A](s: Schema[A]): Unit =
      assertEquals(Json.print(JsonSchema.of(s)), Json.print(legacy(s)))
      assertEquals(Json.print(JsonSchema.of(s, vocabularies = false)), Json.print(legacy(s, vocabularies = false)))
    check(summon[Schema[Order]])
    check(summon[Schema[Shape]])
    check(summon[Schema[Level]])
    check(summon[Schema[Address]])
    check(summon[Schema[Option[List[Vector[Email]]]]])
    check(Schema.SInt); check(Schema.SLong); check(Schema.SDouble); check(Schema.SBool)
    check(Schema.SString); check(Schema.SChar); check(Schema.SBytes)
  }

  // ---- (2) a recursive schema
  final case class Tree(label: String, kids: Vector[Tree])
  given Schema[Tree] = Schema.derived
  enum Expr derives Schema:
    case Num(n: Int)
    case Add(l: Expr, r: Expr)

  test("the hand-rolled fold never returned on a recursive schema") {
    // the claim this stage makes about the past, checked rather than
    // stated: an overflow is an Error, caught here and nowhere else
    val overflowed =
      try { legacy(summon[Schema[Tree]]); false }
      catch case _: StackOverflowError => true
    assert(overflowed, "legacy `of` returned on Tree — the recursion it is claimed to have had is not there")
  }

  test("a recursive product folds to a finite schema: the back edge is $ref, the name is under $defs") {
    val j = JsonSchema.of(summon[Schema[Tree]])
    val printed = Json.print(j)
    assert(printed.contains("\"$ref\":\"#/$defs/Tree\""), printed)
    j match
      case Json.JObj(fs) =>
        val defs = fs.collectFirst { case ("$defs", Json.JObj(d)) => d }.getOrElse(fail("no $defs at the root"))
        assertEquals(defs.map(_._1), Vector("Tree"))
        // the definition is the same object the root inlines, minus the $defs
        assertEquals(Json.print(defs.head._2), Json.print(Json.JObj(fs.filterNot(_._1 == "$defs"))))
      case other => fail(s"expected an object, got $other")
  }

  test("a recursive sum folds the same way, and a self-edge met twice is one $ref each time") {
    val ref = "\"$ref\":\"#/$defs/Expr\""
    def refs(j: Json) = Json.print(j).sliding(ref.length).count(_ == ref)
    JsonSchema.of(summon[Schema[Expr]]) match
      case Json.JObj(fs) =>
        // Add has two Expr edges: both are refs, neither re-folds —
        // twice in the inline root, and twice again in its $defs copy
        assertEquals(refs(Json.JObj(fs.filterNot(_._1 == "$defs"))), 2)
        val defs = fs.collectFirst { case ("$defs", Json.JObj(d)) => d }.getOrElse(fail("no $defs"))
        assertEquals(defs.map(_._1), Vector("Expr"))
        assertEquals(refs(defs.head._2), 2)
      case other => fail(s"expected an object, got $other")
  }

  test("a schema the fold has seen before — through a wrapper or a second field — is NOT a $ref, only a back edge is") {
    // Address appears twice in Pair, and once through an Option; none
    // of those is a cycle, so none becomes a $ref and nothing is
    // declared under $defs: byte-for-byte with the legacy fold
    final case class Pair(a: Address, b: Address, c: Option[Address])
    given Schema[Pair] = Schema.derived
    assertEquals(Json.print(JsonSchema.of(summon[Schema[Pair]])), Json.print(legacy(summon[Schema[Pair]])))
  }

  // ---- (3) identity: each named node folded once, the back edge as ref
  test("a named node is folded exactly once however many edges reach it, and the back edge is ref") {
    val folded = scala.collection.mutable.ListBuffer.empty[String]
    val refs = scala.collection.mutable.ListBuffer.empty[String]
    type K[A] = Unit
    val counting = new Schema.Algebra[K]:
      def int = (); def long = (); def double = (); def bool = (); def string = (); def char = (); def bytes = ()
      def option[A](of: () => Unit) = of()
      def list[A](of: () => Unit) = of()
      def vector[A](of: () => Unit) = of()
      def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[K, Any])]) =
        folded += p.name; fields.foreach(_._2())
      def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[K, A])]) =
        folded += su.name; cases.foreach(_._2())
      def iso[A, B](iso: Schema.SIso[A, B], under: () => Unit) = under()
      def ref[A](name: String) = refs += name: Unit
    Schema.fold(summon[Schema[Expr]])(counting)
    assertEquals(folded.toList, List("Expr", "Num", "Add"))
    assertEquals(refs.toList, List("Expr", "Expr"))
  }

  test("an edge forced twice folds once") {
    var products = 0
    // a LAZY carrier — a value walk: nothing under the root is folded
    // until a value forces the edge, and a finite value bounds the walk
    type K[A] = A => Unit
    val alg = new Schema.Algebra[K]:
      def int = _ => (); def long = _ => (); def double = _ => (); def bool = _ => ()
      def string = _ => (); def char = _ => (); def bytes = _ => ()
      def option[A](of: () => K[A]) = _.foreach(of())
      def list[A](of: () => K[A]) = _.foreach(of())
      def vector[A](of: () => K[A]) = _.foreach(of())
      def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[K, Any])]) =
        products += 1
        // the kernel's own cast, restated: parts(a)(i) IS fields(i)'s X
        def at(e: Schema.Edge[K, Any], v: Any): Unit = e()(v.asInstanceOf[e.X])
        a => p.parts(a).zip(fields).foreach((v, nf) => at(nf._2, v))
      def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[K, A])]) =
        // theCase's own cast, restated: caseOf(a) names the case a IS
        def at(e: Schema.Edge[K, A], a: A): Unit = e()(a.asInstanceOf[e.X])
        a => at(cases(su.caseOf(a))._2, a)
      def iso[A, B](iso: Schema.SIso[A, B], under: () => K[B]) = a => under()(iso.from(a))
      def ref[A](name: String) = _ => ()
    val walk = Schema.fold(summon[Schema[Tree]])(alg)
    assertEquals(products, 1)
    val t = Tree("a", Vector(Tree("b", Vector(Tree("c", Vector.empty))), Tree("d", Vector.empty)))
    walk(t); walk(t)
    // every kids edge back to Tree hit the memo, not a second fold
    assertEquals(products, 1)
  }
