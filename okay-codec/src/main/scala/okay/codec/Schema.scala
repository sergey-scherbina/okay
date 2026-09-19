package okay.codec

import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror
import okay.{Cont, reset, />}

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
    def option[A](o: SOption[A], of: () => F[A]): F[Option[A]]
    def list[A](l: SList[A], of: () => F[A]): F[List[A]]
    def vector[A](v: SVector[A], of: () => F[A]): F[Vector[A]]
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
            case o: SOption[a] => alg.option(o, edge(o.of))
            case l: SList[a] => alg.list(l, edge(l.of))
            case v: SVector[a] => alg.vector(v, edge(v.of))
            case p: SProduct[X] =>
              alg.product(p, p.fields.map((n, f) => (n, fieldEdge(f))))
            case su: SSum[X] =>
              alg.sum(su, su.cases.map((n, c) => (n, caseEdge(c))))
            case iso: SIso[X, b] => alg.iso(iso, edge(iso.under))
          if name != null then inProgress.remove(s): Unit
          done.put(s, out): Unit
          out

    go(s)

  /**
   * A fold, memoised per schema by IDENTITY across calls (specs/
   * schema-fold.md, stage 2): a fold is per SCHEMA, an encode is per
   * VALUE, and the interpreter a lazy algebra answers must be built
   * once and reused, not rebuilt per call. `Key` hashes and compares
   * by reference — `Schema`'s own equality is structural and a
   * recursive schema's would not terminate. One cast, the same table
   * read-back `fold` isolates.
   */
  final class Folded[F[_]](alg: Algebra[F]):
    private final class Key(val s: Schema[?]):
      override def hashCode: Int = System.identityHashCode(s)
      override def equals(o: Any): Boolean = o match
        case k: Key => k.s eq s
        case _ => false
    private val cache = java.util.concurrent.ConcurrentHashMap[Key, Any]()
    def apply[A](s: Schema[A]): F[A] =
      cache.computeIfAbsent(Key(s), _ => fold(s)(alg)).asInstanceOf[F[A]]

  /**
   * The value walk, written ONCE (specs/schema-fold.md, stage 2).
   *
   * `Step[E, A, R]`: given an environment `E` (a `StringBuilder`, a
   * `Cbor.Out`, a form's errors and prefix), a value `A` at nesting
   * `open`, answer `R`. Two roads with one implementation each: `run`
   * is native, `cont` the trampoline, and the ONE place a step
   * descends into a child (`child`, below) takes the road by depth —
   * `Codecs.NativeThreshold`, then `Cont.defer`. This is the split
   * five doors carried by hand on 2026-09-10/11 (`encodeIntoC`,
   * `putC`, `renderC`, `errorsOfC`, ...), lifted out of them: an
   * algebra whose carrier is `Step` cannot have the depth defect, and
   * `Cbor.putC`'s ordering rule — every side effect for one child
   * inside that child's own defer — is `fields`/`elems`' contract
   * here, not each algebra's discipline.
   *
   * `S` is the accumulator a container threads across its children:
   * `Unit` for a side-effecting road (nothing allocated per child),
   * a `Vector[Ui]` for a value-building one.
   */
  trait Step[E, -A, R]:
    def run(e: E, a: A, open: Int): R
    def cont(e: E, a: A, open: Int): R /> R

  object Step:
    /** the algebra carrier a value walk folds to */
    type Walk[E, R] = [X] =>> Step[E, X, R]

    /** the top: a value at depth 0 */
    def walk[E, A, R](s: Step[E, A, R], e: E, a: A): R = s.run(e, a, 0)

    /** THE descent: below the threshold native, at or past it the
      * trampoline — one node forced per iteration of `/`'s loop */
    private def child[E, X, R](s: Step[E, X, R], e: E, x: X, open: Int): R =
      if open >= Codecs.NativeThreshold then reset(s.cont(e, x, open))
      else s.run(e, x, open)

    def leaf[E, A, R](f: (E, A) => R): Step[E, A, R] = new Step[E, A, R]:
      def run(e: E, a: A, open: Int): R = f(e, a)
      def cont(e: E, a: A, open: Int): R /> R = Cont.Pure(f(e, a))

    /** the newtype node: not a level, exactly as it is not one on the
      * read side — `A` travels as `B` at the SAME depth */
    def via[E, A, B, R](from: A => B, under: () => Step[E, B, R]): Step[E, A, R] = new Step[E, A, R]:
      def run(e: E, a: A, open: Int): R = under().run(e, from(a), open)
      def cont(e: E, a: A, open: Int): R /> R = Cont.defer(() => under().cont(e, from(a), open))(r => Cont.Pure(r))

    /** delegate at the SAME depth with the environment and the answer
      * mapped — an option's "(optional)" label, a wrapper that only
      * re-keys; not a level, like `via` */
    def adapt[E, A, R](env: E => E, out: R => R, under: () => Step[E, A, R]): Step[E, A, R] = new Step[E, A, R]:
      def run(e: E, a: A, open: Int): R = out(under().run(env(e), a, open))
      def cont(e: E, a: A, open: Int): R /> R = Cont.defer(() => under().cont(env(e), a, open))(r => Cont.Pure(out(r)))

    def option[E, A, R](none: E => R, some: () => Step[E, A, R]): Step[E, Option[A], R] = new Step[E, Option[A], R]:
      def run(e: E, a: Option[A], open: Int): R = a match
        case Some(x) => child(some(), e, x, open + 1)
        case None => none(e)
      def cont(e: E, a: Option[A], open: Int): R /> R = a match
        case Some(x) => Cont.defer(() => some().cont(e, x, open + 1))(r => Cont.Pure(r))
        case None => Cont.Pure(none(e))

    /**
     * A product's fields: fixed arity, values from `parts` in field
     * order. `kids` are the folded edges at each field's own
     * existential `X`; `parts(a)(i)` IS an `X` (the Mirror's
     * productIterator in field order), re-stated here by the one cast
     * `eachField` isolates for the same reason. `before(e, i, name)`
     * runs before the i-th child (a comma, a key), INSIDE that child's
     * own step on the trampoline road.
     */
    def fields[E, A, R, S](enter: (E, A) => S,
                           parts: A => Seq[Any],
                           kids: Vector[(String, Edge[Walk[E, R], Any])],
                           before: (E, Int, String) => Unit,
                           step: (S, R) => S,
                           close: (E, A, S) => R): Step[E, A, R] = new Step[E, A, R]:
      private def at(e: E, k: Edge[Walk[E, R], Any], v: Any, open: Int): R =
        val st = k()
        child(st, e, v.asInstanceOf[k.X], open)
      private def atC(e: E, k: Edge[Walk[E, R], Any], v: Any, open: Int): R /> R =
        val st = k()
        st.cont(e, v.asInstanceOf[k.X], open)
      def run(e: E, a: A, open: Int): R =
        val ps = parts(a)
        var s = enter(e, a)
        var i = 0
        while i < kids.length do
          val (n, k) = kids(i)
          before(e, i, n)
          s = step(s, at(e, k, ps(i), open + 1))
          i += 1
        close(e, a, s)
      def cont(e: E, a: A, open: Int): R /> R =
        val ps = parts(a)
        def loop(i: Int, s: S): R /> R =
          if i >= kids.length then Cont.Pure(close(e, a, s))
          else
            val (n, k) = kids(i)
            Cont.defer(() => { before(e, i, n); atC(e, k, ps(i), open + 1) })(r => loop(i + 1, step(s, r)))
        loop(0, enter(e, a))

    /** a sum: the ONE case the value is, by `which` (the Mirror's
      * ordinal), typed at that case by the same cast `theCase` isolates */
    def one[E, A, R, S](enter: (E, A) => S,
                        which: A => Int,
                        kids: Vector[(String, Edge[Walk[E, R], A])],
                        before: (E, String) => Unit,
                        close: (E, A, S, R) => R): Step[E, A, R] = new Step[E, A, R]:
      def run(e: E, a: A, open: Int): R =
        val s = enter(e, a)
        val (n, k) = kids(which(a))
        before(e, n)
        val st = k()
        close(e, a, s, child(st, e, a.asInstanceOf[k.X], open + 1))
      def cont(e: E, a: A, open: Int): R /> R =
        val s = enter(e, a)
        val (n, k) = kids(which(a))
        Cont.defer(() => { before(e, n); val st = k(); st.cont(e, a.asInstanceOf[k.X], open + 1) })(r => Cont.Pure(close(e, a, s, r)))

    /**
     * A child chosen by the ALGEBRA, with its own environment and
     * value — for a walk whose value is not the node's own type (a
     * form walks a `Json` by schema, an item's key and label come
     * from the parent). One object per child; the road for the UI
     * pipelines, not for the codecs (`fields`/`elems` allocate none).
     */
    final case class Kid[E, X, R](step: Step[E, X, R], env: E, value: X)

    def node[E, A, R, S](enter: (E, A) => S,
                         kids: (E, A) => Vector[Kid[E, ?, R]],
                         step: (S, R) => S,
                         close: (E, A, S) => R): Step[E, A, R] = new Step[E, A, R]:
      private def at[X](k: Kid[E, X, R], open: Int): R = child(k.step, k.env, k.value, open)
      private def atC[X](k: Kid[E, X, R], open: Int): R /> R = k.step.cont(k.env, k.value, open)
      def run(e: E, a: A, open: Int): R =
        var s = enter(e, a)
        kids(e, a).foreach { k => s = step(s, at(k, open + 1)) }
        close(e, a, s)
      def cont(e: E, a: A, open: Int): R /> R =
        val ks = kids(e, a)
        def loop(i: Int, s: S): R /> R =
          if i >= ks.length then Cont.Pure(close(e, a, s))
          else Cont.defer(() => atC(ks(i), open + 1))(r => loop(i + 1, step(s, r)))
        loop(0, enter(e, a))

    /** a sequence: homogeneous elements, no per-element object */
    def elems[E, A, X, R, S](enter: (E, A) => S,
                             items: A => Iterable[X],
                             each: () => Step[E, X, R],
                             before: (E, Int) => Unit,
                             step: (S, R) => S,
                             close: (E, A, S) => R): Step[E, A, R] = new Step[E, A, R]:
      def run(e: E, a: A, open: Int): R =
        val st = each()
        var s = enter(e, a)
        var i = 0
        items(a).foreach { x =>
          before(e, i)
          s = step(s, child(st, e, x, open + 1))
          i += 1
        }
        close(e, a, s)
      def cont(e: E, a: A, open: Int): R /> R =
        val st = each()
        val it = items(a).iterator
        def loop(i: Int, s: S): R /> R =
          if !it.hasNext then Cont.Pure(close(e, a, s))
          else
            val x = it.next()
            Cont.defer(() => { before(e, i); st.cont(e, x, open + 1) })(r => loop(i + 1, step(s, r)))
        loop(0, enter(e, a))

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
