package okay.codec

import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror

/**
 * The reified shape of a datatype (specs/codecs.md): every derivation
 * — JSON, CBOR, a validator, a Spark encoder — is a CATAMORPHISM over
 * this one structure with its own algebra. Derived once per type via
 * Mirrors (inline, dependency-free); recursion is broken by thunked
 * fields (a self-referential type's schema refers to its own given
 * lazily, so construction terminates).
 */
enum Schema[A]:
  case SInt extends Schema[Int]
  case SLong extends Schema[Long]
  case SDouble extends Schema[Double]
  case SBool extends Schema[Boolean]
  case SString extends Schema[String]
  /** one character — surfaced by deriving okay-ui's Event, whose raw
   * key IS a Char; on the wire a char is a one-character string */
  case SChar extends Schema[Char]
  /**
   * Raw bytes. Not a convenience: CBOR has a first-class byte string
   * (major type 2) and JSON has no bytes at all, so without this every
   * binary payload has to be smuggled through a text field — which is
   * how an embedding came to travel as `List[Double]`, nine bytes and
   * one boxed object per component.
   *
   * `Array[Byte]` carries reference equality, so a product holding one
   * is not a value for `==`. That is the honest cost of not copying.
   */
  case SBytes extends Schema[Array[Byte]]
  case SOption[A](of: () => Schema[A]) extends Schema[Option[A]]
  case SList[A](of: () => Schema[A]) extends Schema[List[A]]
  /** the OTHER sequence this stack actually uses — Ui children,
   * codec fields, chunk contents are Vectors; smuggling them through
   * List cost a conversion at every derivation edge (codec-vector) */
  case SVector[A](of: () => Schema[A]) extends Schema[Vector[A]]
  case SProduct[A](name: String, fields: Vector[(String, () => Schema[?])],
                   make: Seq[Any] => A, parts: A => Seq[Any],
                   /** aligned with fields; a decoder falls back here
                    * when the wire lacks the field (codec-defaults) */
                   defaults: Vector[Option[() => Any]] = Vector.empty) extends Schema[A]
  case SSum[A](name: String, cases: Vector[(String, () => Schema[? <: A])],
               caseOf: A => Int) extends Schema[A]
  /** the newtype node (codec-iso): A travels as B — encode is `from`
   * then under's encode, decode is under's decode then `to`, and a
   * Left from `to` is a decode error like any other. To every
   * algebra the wrapper does not exist, which is the point. */
  case SIso[A, B](under: () => Schema[B],
                  to: B => Either[String, A],
                  from: A => B)
                 (/** the finite vocabulary of `B` this wrapper admits, when
                   * there is one (`Schema.enumeration`): every algebra still
                   * sees the wrapper as `under`, and only a DECLARATION —
                   * the JSON Schema — reads it, as `"enum"`. A second
                   * parameter list, so `SIso(u, to, from)` patterns stay
                   * three-armed everywhere. */
                  val vocabulary: Option[Vector[B]] = None) extends Schema[A]

object Schema {

  /** the PRODUCT kernel, once: `parts` is the Mirror's productIterator
   * in field order, so the i-th value IS the i-th field's type — a
   * codec sees each field at that type through f and never casts */
  extension [A](p: SProduct[A])
    def eachField[R](a: A)(f: [X] => (String, Schema[X], X) => R): Vector[R] =
      def one[X](name: String, sc: Schema[X], v: Any): R = f(name, sc, v.asInstanceOf[X])
      p.parts(a).toVector.zip(p.fields).map((v, fld) => one(fld._1, fld._2(), v))

  /** the DEFAULTS kernel, once: `defaults` is aligned with `fields`
   * (Defaults.of builds it from the Mirror in field order), so the
   * i-th default IS the i-th field's type */
  extension [A](p: SProduct[A])
    def defaultAt[R](i: Int)(f: [X] => (Schema[X], X) => R): Option[R] =
      def one[X](sc: Schema[X], d: () => Any): R = f(sc, d().asInstanceOf[X])
      p.defaults.lift(i).flatten.map(d => one(p.fields(i)._2(), d))

  /** the SUM kernel, once: `caseOf` is the Mirror's ordinal, so the
   * value IS that case's type — a codec sees it at that type through f */
  extension [A](su: SSum[A])
    def theCase[R](a: A)(f: [X <: A] => (String, Schema[X], X) => R): R =
      val (name, sc) = su.cases(su.caseOf(a))
      def one[X <: A](sc: Schema[X]): R = f(name, sc, a.asInstanceOf[X])
      one(sc())

  /**
   * A lazily folded edge at the schema's own existential type — a
   * product field's `?`, a sum case's `? <: A`. A type MEMBER, not
   * `F[?]`: applying an abstract `F` to a wildcard is unreducible in
   * Scala 3. An algebra sees `X` abstract, exactly as the GADT keeps
   * it, and can apply `F[X]` only to an `X` — which, for a value
   * algebra, is the `Any` a product's `parts` hands it re-stated at
   * the field's type, the cast `eachField` already isolates. Forcing
   * twice folds once.
   */
  trait Edge[F[_], +B]:
    type X <: B
    def apply(): F[X]

  trait Algebra[F[_]]:
    def int: F[Int]
    def long: F[Long]
    def double: F[Double]
    def bool: F[Boolean]
    def string: F[String]
    def char: F[Char]
    def bytes: F[Array[Byte]]
    def option[A](of: () => F[A]): F[Option[A]]
    def list[A](of: () => F[A]): F[List[A]]
    def vector[A](of: () => F[A]): F[Vector[A]]
    /** the node itself comes along with its folded edges — a
      * PARAmorphism, not a plain cata: a JSON Schema renders a field's
      * DEFAULT with the field's own schema (`defaultAt`) and an
      * enumeration's vocabulary with `under`'s, and a value algebra
      * needs `parts`/`make`/`theCase` — all of which live on the node */
    def product[A](p: SProduct[A], fields: Vector[(String, Edge[F, Any])]): F[A]
    def sum[A](su: SSum[A], cases: Vector[(String, Edge[F, A])]): F[A]
    def iso[A, B](iso: SIso[A, B], under: () => F[B]): F[A]
    /** a NAMED node (product or sum) met again while it is still being
      * folded — the back edge of a recursive type. Only a strict
      * algebra ever sees this; a lazy one forces the edge after the
      * node finished and gets the memoised node instead. */
    def ref[A](name: String): F[A]

  /**
   * The catamorphism the header promises (specs/schema-fold.md,
   * stage 1). Memoised by schema IDENTITY: `once` makes every edge
   * answer the same instance, a `given ... = Schema.derived` is
   * evaluated once, so the `Tree` inside `kids` IS the `Tree` at the
   * root — and identity is how the fold knows it is back there. A
   * schema whose edges answer fresh instances per call (a `def` given,
   * a thunk built without `once`) folds to an infinite unfolding; a
   * derived one never does.
   *
   * Two tables: `done` holds finished nodes (the second `go(Tree)`,
   * at value time, answers the same `F[Tree]` — no re-fold per value
   * node, the defect `schema-thunks-fresh-instances` already met once);
   * `inProgress` holds the named nodes on the current path, so a
   * strict algebra forcing an edge back into one gets `ref(name)`
   * instead of an infinite descent.
   *
   * Three casts, each restoring what erasure took and nothing more
   * (no-casts-without-necessity): `remembered` reads back the `F[X]`
   * stored under its own `Schema[X]`; `fieldEdge`/`caseEdge` re-state
   * a thunk's type the way `derived` itself does for a sum's cases —
   * the schema stored the thunk as `Schema[?]`, so the fold can only
   * name it as a member. No value is ever cast.
   */
  def fold[A, F[_]](s: Schema[A])(alg: Algebra[F]): F[A] =
    val done = java.util.IdentityHashMap[Schema[?], Any]()
    val inProgress = java.util.IdentityHashMap[Schema[?], String]()

    def remembered[X](s: Schema[X]): Option[F[X]] =
      Option(done.get(s)).map(_.asInstanceOf[F[X]])

    def edge[X](s: () => Schema[X]): () => F[X] =
      lazy val v = go(s())
      () => v
    def fieldEdge(s: () => Schema[?]): Edge[F, Any] = new Edge[F, Any]:
      type X = Any
      lazy val v: F[Any] = go(s().asInstanceOf[Schema[Any]])
      def apply(): F[Any] = v
    def caseEdge[A](s: () => Schema[? <: A]): Edge[F, A] = new Edge[F, A]:
      type X = A
      lazy val v: F[A] = go(s().asInstanceOf[Schema[A]])
      def apply(): F[A] = v

    def go[X](s: Schema[X]): F[X] = remembered(s) match
      case Some(f) => f
      case None =>
        val name = s match
          case p: SProduct[?] => p.name
          case su: SSum[?] => su.name
          case _ => null
        if name != null && inProgress.containsKey(s) then alg.ref[X](name)
        else
          if name != null then inProgress.put(s, name): Unit
          val out: F[X] = s match
            case SInt => alg.int
            case SLong => alg.long
            case SDouble => alg.double
            case SBool => alg.bool
            case SString => alg.string
            case SChar => alg.char
            case SBytes => alg.bytes
            case SOption(of) => alg.option(edge(of))
            case SList(of) => alg.list(edge(of))
            case SVector(of) => alg.vector(edge(of))
            case p: SProduct[X] =>
              alg.product(p, p.fields.map((n, f) => (n, fieldEdge(f))))
            case su: SSum[X] =>
              alg.sum(su, su.cases.map((n, c) => (n, caseEdge(c))))
            case iso: SIso[X, b] => alg.iso(iso, edge(iso.under))
          if name != null then inProgress.remove(s): Unit
          done.put(s, out): Unit
          out

    go(s)

  given Schema[Int] = Schema.SInt
  given Schema[Long] = Schema.SLong
  given Schema[Double] = Schema.SDouble
  given Schema[Boolean] = Schema.SBool
  given Schema[String] = Schema.SString
  given Schema[Char] = Schema.SChar
  given Schema[Array[Byte]] = Schema.SBytes

  /**
   * A thunk that answers the SAME instance every time (schema-thunks-
   * once). Every edge of a schema is lazy — that is how a recursive
   * type's schema terminates — but a lazy edge is not a memoised one:
   * `() => summonInline[Schema[h]]` re-expanded the derivation on every
   * call for a subtype without a given of its own, so a sum's case
   * schema was a fresh instance per value encoded (an allocation the
   * interpreter paid per field per value) and anything keyed by
   * identity — a staged generator's node table — could not find the
   * child it had just seen. By-name in, `lazy val` behind the thunk:
   * still nothing is forced at construction, and once forced it stays.
   */
  def once[X](s: => Schema[X]): () => Schema[X] =
    lazy val v = s
    () => v

  /** a total wrapper — a newtype travels as what it wraps */
  def wrap[A, B](to: B => A, from: A => B)(using s: => Schema[B]): Schema[A] =
    Schema.SIso(once(s), b => Right(to(b)), from)()

  /** a refining wrapper — a Left is a decode error naming itself */
  def refine[A, B](to: B => Either[String, A], from: A => B)(using s: => Schema[B]): Schema[A] =
    Schema.SIso(once(s), to, from)()

  /**
   * A refinement over a FINITE vocabulary (codec-jsonschema-refinement-
   * enum): `values` are the only `A`s, each spelt as `name(a)` on the
   * wire, an unrecognised spelling a decode error naming itself. On
   * every wire it is `refine`; in a JSON Schema it is
   * `{"type": ..., "enum": [...]}` — so a contract or a tool
   * declaration carries the vocabulary a prompt used to have to state.
   */
  def enumeration[A, B](values: Vector[A], name: A => B)(using s: => Schema[B]): Schema[A] =
    val byName = values.map(a => name(a) -> a)
    vocabulary[A, B](byName.map(_._1),
      b => byName.collectFirst { case (n, a) if n == b => a }
        .toRight(s"unknown value '$b'; one of: ${byName.map(_._1).mkString(", ")}"),
      name)

  /** `refine` that DECLARES its vocabulary: the same `to`/`from` as
   * any refinement (so a decoder may accept more spellings than the
   * declaration lists — `Conf` reads `High` and `high` alike), plus
   * the `names` a JSON Schema will show as `enum` */
  def vocabulary[A, B](names: Vector[B], to: B => Either[String, A], from: A => B)(using s: => Schema[B]): Schema[A] =
    Schema.SIso[A, B](once(s), to, from)(Some(names))

  given [A](using s: => Schema[A]): Schema[Option[A]] = Schema.SOption(once(s))
  given [A](using s: => Schema[A]): Schema[List[A]] = Schema.SList(once(s))
  given [A](using s: => Schema[A]): Schema[Vector[A]] = Schema.SVector(once(s))

  private inline def thunks[T <: Tuple]: List[() => Schema[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) => once(summonInline[Schema[h]]) :: thunks[t]

  /** derive from the Mirror: products become named fields, sums named
   * cases; write `given Schema[T] = Schema.derived` (or `derives`) */
  inline given derived[A](using m: Mirror.Of[A]): Schema[A] =
    inline m match
      case p: Mirror.ProductOf[A] =>
        val labels = constValueTuple[p.MirroredElemLabels].toList.map(_.toString)
        val fields = labels.zip(thunks[p.MirroredElemTypes]).toVector
        Schema.SProduct(
          constValueTuple[Tuple1[p.MirroredLabel]].head.toString,
          fields,
          xs => p.fromProduct(Tuple.fromArray(xs.toArray)),
          a => a.asInstanceOf[Product].productIterator.toSeq,
          Defaults.of[A])
      case s: Mirror.SumOf[A] =>
        val labels = constValueTuple[s.MirroredElemLabels].toList.map(_.toString)
        // the Mirror's claim, once: a sum's element types are its
        // subtypes (the compiler derived them so; the inline match on
        // the tuple type cannot see the bound)
        val cases = thunks[s.MirroredElemTypes].map(_.asInstanceOf[() => Schema[? <: A]])
        Schema.SSum(
          constValueTuple[Tuple1[s.MirroredLabel]].head.toString,
          labels.zip(cases).toVector,
          a => s.ordinal(a))
}
