package okay2.codec

import scala.language.experimental.macros
import okay2.{Cont, reset, />}

/**
 * A type's shape as data (okay-codec's Schema.scala): the scalars, the
 * three containers, products, sums and a checked iso — one description
 * that every dialect folds.
 *
 * Scala 3 matches this enum with GADT refinement: `case SInt =>` makes
 * the answer `F[Int]` where `F[A]` was asked. Scala 2 does not refine a
 * type parameter on a case OBJECT pattern, so the dispatch here is a
 * VISITOR, `visit(Visit[F]): F[A]`, which is the same refinement typed
 * by the case itself: `SInt.visit(v) = v.int`. Every fold and every
 * decoder below dispatches through it, and none of them casts.
 */
sealed abstract class Schema[A] {
  def visit[F[_]](v: Schema.Visit[F]): F[A]
}

object Schema extends SchemaDerivation {

  case object SInt extends Schema[Int] { def visit[F[_]](v: Visit[F]): F[Int] = v.int }
  case object SLong extends Schema[Long] { def visit[F[_]](v: Visit[F]): F[Long] = v.long }
  case object SDouble extends Schema[Double] { def visit[F[_]](v: Visit[F]): F[Double] = v.double }
  case object SBool extends Schema[Boolean] { def visit[F[_]](v: Visit[F]): F[Boolean] = v.bool }
  case object SString extends Schema[String] { def visit[F[_]](v: Visit[F]): F[String] = v.string }
  case object SChar extends Schema[Char] { def visit[F[_]](v: Visit[F]): F[Char] = v.char }
  case object SBytes extends Schema[Array[Byte]] { def visit[F[_]](v: Visit[F]): F[Array[Byte]] = v.bytes }
  case object SBigInt extends Schema[BigInt] { def visit[F[_]](v: Visit[F]): F[BigInt] = v.bigInt }

  final case class SOption[A](of: () => Schema[A]) extends Schema[Option[A]] {
    def visit[F[_]](v: Visit[F]): F[Option[A]] = v.option(this)
  }
  final case class SList[A](of: () => Schema[A]) extends Schema[List[A]] {
    def visit[F[_]](v: Visit[F]): F[List[A]] = v.list(this)
  }
  final case class SVector[A](of: () => Schema[A]) extends Schema[Vector[A]] {
    def visit[F[_]](v: Visit[F]): F[Vector[A]] = v.vector(this)
  }

  /** a product: its fields by name, in declaration order, as thunks (a
   * recursive type's field is its own schema), `make` from the erased
   * parts, `parts` back out of a value, and each field's declared
   * default where it has one */
  final case class SProduct[A](name: String, fields: Vector[(String, () => Schema[_])],
                               make: Seq[Any] => A, parts: A => Seq[Any],
                               defaults: Vector[Option[() => Any]] = Vector.empty) extends Schema[A] {
    def visit[F[_]](v: Visit[F]): F[A] = v.product(this)
  }

  /** a sum: its cases by name, in declaration order, and which one a
   * value is */
  final case class SSum[A](name: String, cases: Vector[(String, () => Schema[_ <: A])],
                           caseOf: A => Int) extends Schema[A] {
    def visit[F[_]](v: Visit[F]): F[A] = v.sum(this)
  }

  /** `A` as a checked view of `B`; `vocabulary` is the finite set of
   * `B` it admits, when there is one (an enumeration), which a schema
   * export states and the codecs never need */
  final case class SIso[A, B](under: () => Schema[B], to: B => Either[String, A], from: A => B)
                             (val vocabulary: Option[Vector[B]] = None) extends Schema[A] {
    def visit[F[_]](v: Visit[F]): F[A] = v.iso(this)
  }

  /** one method per case: what `visit` calls, and so what a `match`
   * with GADT refinement would have been */
  trait Visit[F[_]] {
    def int: F[Int]
    def long: F[Long]
    def double: F[Double]
    def bool: F[Boolean]
    def string: F[String]
    def char: F[Char]
    def bytes: F[Array[Byte]]
    def bigInt: F[BigInt]
    def option[A](o: SOption[A]): F[Option[A]]
    def list[A](l: SList[A]): F[List[A]]
    def vector[A](v: SVector[A]): F[Vector[A]]
    def product[A](p: SProduct[A]): F[A]
    def sum[A](su: SSum[A]): F[A]
    def iso[A, B](i: SIso[A, B]): F[A]
  }

  /** a function polymorphic in the field's type — Scala 3's
   * `[X] => (String, Schema[X], X) => R` */
  trait FieldFn[R] { def apply[X](name: String, s: Schema[X], x: X): R }
  trait DefaultFn[R] { def apply[X](s: Schema[X], x: X): R }
  trait CaseFn[A, R] { def apply[X <: A](name: String, s: Schema[X], x: X): R }

  implicit final class ProductOps[A](private val p: SProduct[A]) extends AnyVal {
    /** each field with its schema and its value, typed together */
    def eachField[R](a: A)(f: FieldFn[R]): Vector[R] =
      p.parts(a).toVector.zip(p.fields).map { case (v, (name, sc)) => Erased.field(name, sc(), v, f) }

    /** the declared default of field `i`, typed with its schema */
    def defaultAt[R](i: Int)(f: DefaultFn[R]): Option[R] =
      p.defaults.lift(i).flatten.map(d => Erased.default(p.fields(i)._2(), d(), f))
  }

  implicit final class SumOps[A](private val su: SSum[A]) extends AnyVal {
    /** the case a value is, with its schema, typed together */
    def theCase[R](a: A)(f: CaseFn[A, R]): R = {
      val (name, sc) = su.cases(su.caseOf(a))
      f(name, Erased.schema[A](sc()), a)
    }
  }

  /** the ONE place a product's erased part meets its field's schema:
   * `parts` and `make` speak `Any` because a product's fields are a
   * heterogeneous list, and the part at index i IS the field at index
   * i's type — the derivation writes both from one field list */
  private object Erased {
    def field[X, R](name: String, sc: Schema[X], v: Any, f: FieldFn[R]): R = f(name, sc, v.asInstanceOf[X])
    def default[X, R](sc: Schema[X], d: Any, f: DefaultFn[R]): R = f(sc, d.asInstanceOf[X])
    def part[X](v: Any): X = v.asInstanceOf[X]
    def schema[X](s: Schema[_]): Schema[X] = s.asInstanceOf[Schema[X]]
  }

  /** an edge of the folded graph: the carrier of ONE field or case,
   * whose type is known only to the edge itself */
  trait Edge[F[_], +B] {
    type X <: B
    def apply(): F[X]
  }

  /** a fold's cases — `fold` hands each container its children's
   * carriers as thunks, so a recursive schema folds to a cyclic carrier
   * instead of an infinite one */
  trait Algebra[F[_]] {
    def int: F[Int]
    def long: F[Long]
    def double: F[Double]
    def bool: F[Boolean]
    def string: F[String]
    def char: F[Char]
    def bytes: F[Array[Byte]]
    def bigInt: F[BigInt]
    def option[A](o: SOption[A], of: () => F[A]): F[Option[A]]
    def list[A](l: SList[A], of: () => F[A]): F[List[A]]
    def vector[A](v: SVector[A], of: () => F[A]): F[Vector[A]]
    def product[A](p: SProduct[A], fields: Vector[(String, Edge[F, Any])]): F[A]
    def sum[A](su: SSum[A], cases: Vector[(String, Edge[F, A])]): F[A]
    def iso[A, B](iso: SIso[A, B], under: () => F[B]): F[A]
    /** a back edge reached while its target is still being folded (a
     * strict carrier's only way to meet recursion) */
    def ref[A](name: String): F[A]
  }

  /** fold a schema with an algebra, each node once (by identity) */
  def fold[A, F[_]](s: Schema[A])(alg: Algebra[F]): F[A] = {
    val done = new java.util.IdentityHashMap[Schema[_], Any]()
    val inProgress = new java.util.IdentityHashMap[Schema[_], String]()

    // the memo is heterogeneous, keyed by the schema's identity: the
    // value stored at `s` was `go(s)`, an F[X] for that very X
    def remembered[X](s: Schema[X]): Option[F[X]] =
      Option(done.get(s)).map(_.asInstanceOf[F[X]])

    def edge[X](s: () => Schema[X]): () => F[X] = {
      lazy val v = go(s())
      () => v
    }
    // a field's thunk is typed `() => Schema[_]` and a case's
    // `() => Schema[_ <: B]`: a function whose every call MAY answer a
    // different type, which no type parameter can open. It is one schema
    // (`once`), so each edge takes it at the type its parts are read at
    // (Any for a field, B for a case) — `Erased.schema`, the one cast
    def fieldEdge(f: () => Schema[_]): Edge[F, Any] = new Edge[F, Any] {
      type X = Any
      lazy val v: F[Any] = go(Erased.schema[Any](f()))
      def apply(): F[Any] = v
    }
    def caseEdge[B](c: () => Schema[_ <: B]): Edge[F, B] = new Edge[F, B] {
      type X = B
      lazy val v: F[B] = go(Erased.schema[B](c()))
      def apply(): F[B] = v
    }

    def go[X](s: Schema[X]): F[X] = remembered(s) match {
      case Some(f) => f
      case None =>
        val name = s match {
          case p: SProduct[_] => p.name
          case su: SSum[_] => su.name
          case _ => null
        }
        if (name != null && inProgress.containsKey(s)) alg.ref[X](name)
        else {
          if (name != null) inProgress.put(s, name)
          val out: F[X] = s.visit(new Visit[F] {
            def int = alg.int
            def long = alg.long
            def double = alg.double
            def bool = alg.bool
            def string = alg.string
            def char = alg.char
            def bytes = alg.bytes
            def bigInt = alg.bigInt
            def option[B](o: SOption[B]) = alg.option(o, edge(o.of))
            def list[B](l: SList[B]) = alg.list(l, edge(l.of))
            def vector[B](v: SVector[B]) = alg.vector(v, edge(v.of))
            def product[B](p: SProduct[B]) = alg.product(p, p.fields.map { case (n, f) => (n, fieldEdge(f)) })
            def sum[B](su: SSum[B]) = alg.sum(su, su.cases.map { case (n, c) => (n, caseEdge[B](c)) })
            def iso[B, C](i: SIso[B, C]) = alg.iso(i, edge(i.under))
          })
          if (name != null) inProgress.remove(s)
          done.put(s, out)
          out
        }
    }

    go(s)
  }

  /** a fold remembered per schema (by identity), so a codec folds its
   * schema once and not per value */
  /** a schema by identity, as a map key */
  private final class Key(val s: Schema[_]) {
    override def hashCode: Int = System.identityHashCode(s)
    override def equals(o: Any): Boolean = o match {
      case k: Key => k.s eq s
      case _ => false
    }
  }

  final class Folded[F[_]](alg: Algebra[F]) {
    private val cache = new java.util.concurrent.ConcurrentHashMap[Key, Any]()
    // keyed by the schema's identity, as `fold`'s own memo
    def apply[A](s: Schema[A]): F[A] =
      cache.computeIfAbsent(new Key(s), _ => fold(s)(alg)).asInstanceOf[F[A]]
  }

  /**
   * A walk over a VALUE, native below `Codecs.NativeThreshold` open
   * containers and a `Cont.defer` trampoline past it — so a document
   * as deep as the heap allows encodes without the JVM stack.
   */
  trait Step[E, -A, R] {
    def run(e: E, a: A, open: Int): R
    def cont(e: E, a: A, open: Int): R /> R
  }

  object Step {
    def walk[E, A, R](s: Step[E, A, R], e: E, a: A): R = s.run(e, a, 0)

    private def child[E, X, R](s: Step[E, X, R], e: E, x: X, open: Int): R =
      if (open >= Codecs.NativeThreshold) reset(s.cont(e, x, open))
      else s.run(e, x, open)

    def leaf[E, A, R](f: (E, A) => R): Step[E, A, R] = new Step[E, A, R] {
      def run(e: E, a: A, open: Int): R = f(e, a)
      def cont(e: E, a: A, open: Int): R /> R = Cont.Pure(f(e, a))
    }

    def via[E, A, B, R](from: A => B, under: () => Step[E, B, R]): Step[E, A, R] = new Step[E, A, R] {
      def run(e: E, a: A, open: Int): R = under().run(e, from(a), open)
      def cont(e: E, a: A, open: Int): R /> R = Cont.defer(() => under().cont(e, from(a), open))((r: R) => Cont.Pure[R, R](r))
    }

    def adapt[E, A, R](env: E => E, out: R => R, under: () => Step[E, A, R]): Step[E, A, R] = new Step[E, A, R] {
      def run(e: E, a: A, open: Int): R = out(under().run(env(e), a, open))
      def cont(e: E, a: A, open: Int): R /> R = Cont.defer(() => under().cont(env(e), a, open))((r: R) => Cont.Pure[R, R](out(r)))
    }

    def option[E, A, R](none: E => R, some: () => Step[E, A, R]): Step[E, Option[A], R] = new Step[E, Option[A], R] {
      def run(e: E, a: Option[A], open: Int): R = a match {
        case Some(x) => child(some(), e, x, open + 1)
        case None => none(e)
      }
      def cont(e: E, a: Option[A], open: Int): R /> R = a match {
        case Some(x) => Cont.defer(() => some().cont(e, x, open + 1))((r: R) => Cont.Pure[R, R](r))
        case None => Cont.Pure(none(e))
      }
    }

    /** `F` is the fold's carrier, a `Step` per type: bounded rather than
     * a type projection, which Scala 2 does not unify with an alias */
    def fields[E, A, R, S, F[X] <: Step[E, X, R]](enter: (E, A) => S,
                           parts: A => Seq[Any],
                           kids: Vector[(String, Edge[F, Any])],
                           before: (E, Int, String) => Unit,
                           step: (S, R) => S,
                           close: (E, A, S) => R): Step[E, A, R] = new Step[E, A, R] {
      private def at(e: E, k: Edge[F, Any], v: Any, open: Int): R =
        child(k(), e, Erased.part[k.X](v), open)
      private def atC(e: E, k: Edge[F, Any], v: Any, open: Int): R /> R =
        k().cont(e, Erased.part[k.X](v), open)
      def run(e: E, a: A, open: Int): R = {
        val ps = parts(a)
        var s = enter(e, a)
        var i = 0
        while (i < kids.length) {
          val (n, k) = kids(i)
          before(e, i, n)
          s = step(s, at(e, k, ps(i), open + 1))
          i += 1
        }
        close(e, a, s)
      }
      def cont(e: E, a: A, open: Int): R /> R = {
        val ps = parts(a)
        def loop(i: Int, s: S): R /> R =
          if (i >= kids.length) Cont.Pure(close(e, a, s))
          else {
            val (n, k) = kids(i)
            Cont.defer(() => { before(e, i, n); atC(e, k, ps(i), open + 1) })((r: R) => loop(i + 1, step(s, r)))
          }
        loop(0, enter(e, a))
      }
    }

    def one[E, A, R, S, F[X] <: Step[E, X, R]](enter: (E, A) => S,
                        which: A => Int,
                        kids: Vector[(String, Edge[F, A])],
                        before: (E, String) => Unit,
                        close: (E, A, S, R) => R): Step[E, A, R] = new Step[E, A, R] {
      def run(e: E, a: A, open: Int): R = {
        val s = enter(e, a)
        val (n, k) = kids(which(a))
        before(e, n)
        close(e, a, s, child(k(), e, Erased.part[k.X](a), open + 1))
      }
      def cont(e: E, a: A, open: Int): R /> R = {
        val s = enter(e, a)
        val (n, k) = kids(which(a))
        Cont.defer(() => { before(e, n); k().cont(e, Erased.part[k.X](a), open + 1) })((r: R) => Cont.Pure[R, R](close(e, a, s, r)))
      }
    }

    def elems[E, A, X, R, S](enter: (E, A) => S,
                             items: A => Iterable[X],
                             each: () => Step[E, X, R],
                             before: (E, Int) => Unit,
                             step: (S, R) => S,
                             close: (E, A, S) => R): Step[E, A, R] = new Step[E, A, R] {
      def run(e: E, a: A, open: Int): R = {
        val st = each()
        var s = enter(e, a)
        var i = 0
        items(a).foreach { x =>
          before(e, i)
          s = step(s, child(st, e, x, open + 1))
          i += 1
        }
        close(e, a, s)
      }
      def cont(e: E, a: A, open: Int): R /> R = {
        val st = each()
        val it = items(a).iterator
        def loop(i: Int, s: S): R /> R =
          if (!it.hasNext) Cont.Pure(close(e, a, s))
          else {
            val x = it.next()
            Cont.defer(() => { before(e, i); st.cont(e, x, open + 1) })((r: R) => loop(i + 1, step(s, r)))
          }
        loop(0, enter(e, a))
      }
    }
  }

  implicit val int: Schema[Int] = SInt
  implicit val long: Schema[Long] = SLong
  implicit val double: Schema[Double] = SDouble
  implicit val bool: Schema[Boolean] = SBool
  implicit val string: Schema[String] = SString
  implicit val char: Schema[Char] = SChar
  implicit val bytes: Schema[Array[Byte]] = SBytes
  implicit val bigInt: Schema[BigInt] = SBigInt

  implicit def option[A](implicit s: => Schema[A]): Schema[Option[A]] = SOption(once(s))
  implicit def list[A](implicit s: => Schema[A]): Schema[List[A]] = SList(once(s))
  implicit def vector[A](implicit s: => Schema[A]): Schema[Vector[A]] = SVector(once(s))

  /** a schema evaluated at most once, on first use */
  def once[X](s: => Schema[X]): () => Schema[X] = {
    lazy val v = s
    () => v
  }

  /** `A` as a total view of `B` */
  def wrap[A, B](to: B => A, from: A => B)(implicit s: => Schema[B]): Schema[A] =
    SIso[A, B](once(s), b => Right(to(b)), from)()

  /** `A` as a checked view of `B` */
  def refine[A, B](to: B => Either[String, A], from: A => B)(implicit s: => Schema[B]): Schema[A] =
    SIso[A, B](once(s), to, from)()

  /** a finite set of values, each by its name */
  def enumeration[A, B](values: Vector[A], name: A => B)(implicit s: => Schema[B]): Schema[A] = {
    val byName = values.map(a => name(a) -> a)
    vocabulary[A, B](byName.map(_._1),
      b => byName.collectFirst { case (n, a) if n == b => a }
        .toRight(s"unknown value '$b'; one of: ${byName.map(_._1).mkString(", ")}"),
      name)(s)
  }

  /** a checked view whose admitted `B`s are listed */
  def vocabulary[A, B](names: Vector[B], to: B => Either[String, A], from: A => B)(implicit s: => Schema[B]): Schema[A] =
    SIso[A, B](once(s), to, from)(Some(names))
}

/**
 * Derivation, at LOWER priority than the instances above: a case class
 * is a product, a sealed trait (or abstract class) a sum. Scala 3 reads
 * a `Mirror`; here a blackbox macro reads the class (`SchemaMacro`).
 * Types under `scala.` are never derived, so `Option` stays the option
 * and never becomes the sum `Some | None`.
 */
trait SchemaDerivation {
  implicit def derived[A]: Schema[A] = macro SchemaMacro.derive[A]
}
