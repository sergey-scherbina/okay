package okay2.codec

import okay2.Applicative
import okay2.optics.Plate
import okay2.optics.Optic._

/**
 * Optics over `Json` (okay-codec's JsonOptic, specs/optics.md stage 1).
 * They are the other carrier: for a derived `Schema[A]`, a field's lens
 * on the VALUE and that field's optic on its JSON commute with the
 * codec — an edit to the value and the same edit to its wire shape
 * cannot drift.
 *
 * The lawful one is `at`, whose focus is an `Option[Json]`: absent is
 * `None`, `set(None)` removes, `set(Some(v))` inserts or replaces. The
 * ones a path is written with — `field`, `index`, `caseOf` — are
 * affines derived from it (or built directly): a lens that CREATES a
 * missing field breaks GetPut.
 *
 * Totality: applied to a Json of the wrong shape every one of these is
 * the identity and previews nothing.
 *
 * ONE LAW HOLDS MODULO FIELD ORDER: `at`'s PutPut is exact except when
 * the first put removed the field, because a removal loses where the
 * field was and the next insert appends (RFC 8259's objects are
 * unordered; `JObj` keeps the codec's order).
 *
 * What differs from Scala 3: the traversals are `Walk`s (Scala 2 has no
 * polymorphic function type), and the plate is an implicit object.
 */
object JsonOptic {

  /** the field as an `Option`: THE lawful lens */
  def at(name: String): Lens[Json, Json, Option[Json], Option[Json]] =
    Lens[Json, Json, Option[Json], Option[Json]](
      {
        case Json.JObj(fs) => fs.collectFirst { case (n, v) if n == name => v }
        case _ => None
      },
      (j, ov) => j match {
        case Json.JObj(fs) => ov match {
          case Some(v) =>
            if (fs.exists(_._1 == name)) Json.JObj(fs.map { case (n, old) => if (n == name) (n, v) else (n, old) })
            else Json.JObj(fs :+ (name -> v))
          case None => Json.JObj(fs.filterNot(_._1 == name))
        }
        case other => other
      })

  private val identityLens: Lens[Json, Json, Json, Json] = Lens[Json, Json, Json, Json](j => j, (_, v) => v)

  /**
   * A path that CREATES what is missing on the way down, and is a
   * lawful lens while doing it: each step is `at(name)` composed with
   * `Iso.non(d)`, so an absent field reads as `d` and writing `d` back
   * removes it again. Each step carries its own default — an object, an
   * empty array, a zero — because only the schema knows what absence
   * means. Built by a fold, one lens per step of the caller's list.
   */
  def creating(steps: List[(String, Json)]): Lens[Json, Json, Json, Json] =
    steps.foldRight(identityLens) { case ((name, d), inner) =>
      val here = at(name).andThen(Iso.non(d)).andThen(inner)
      new Lens[Json, Json, Json, Json] {
        def apply[P[_, _]](p: P[Json, Json])(implicit P: Strong[P]): P[Json, Json] = here[P](p)(P)
      }
    }

  /** the same, when every level is an object */
  def creatingObjects(names: List[String]): Lens[Json, Json, Json, Json] =
    creating(names.map(n => (n, Json.JObj(Vector.empty))))

  /** the field when it is there: `at(name)` through `Some` */
  def field(name: String): Affine[Json, Json, Json, Json] = {
    // composed WITHOUT the expected type in view: Scala 2 would infer
    // `andThen`'s constraint from it and refuse the prism
    val o = at(name).andThen(Prism.some[Json, Json])
    o
  }

  /** two affines in sequence, typed as an affine (see `field`) */
  private def seq(a: Affine[Json, Json, Json, Json], b: Affine[Json, Json, Json, Json]): Affine[Json, Json, Json, Json] = {
    val o = a.andThen(b)
    o
  }

  /** the i-th element of an array, when the array has one */
  def index(i: Int): Affine[Json, Json, Json, Json] =
    Affine[Json, Json, Json, Json](
      {
        case Json.JArr(vs) if i >= 0 && i < vs.length => Right(vs(i))
        case other => Left(other)
      },
      (j, v) => j match {
        case Json.JArr(vs) if i >= 0 && i < vs.length => Json.JArr(vs.updated(i, v))
        case other => other
      })

  /** the codec's sum shape, `{"Case": value}`, when the case is this one */
  def caseOf(name: String): Affine[Json, Json, Json, Json] =
    Affine[Json, Json, Json, Json](
      {
        case Json.JObj(Vector((n, v))) if n == name => Right(v)
        case other => Left(other)
      },
      (j, v) => j match {
        case Json.JObj(Vector((n, _))) if n == name => Json.JObj(Vector(n -> v))
        case other => other
      })

  /** every element of an array, in order */
  val values: Traversal[Json, Json, Json, Json] = Traversal(new Walk[Json, Json, Json, Json] {
    def apply[F[_]](f: Json => F[Json])(implicit F: Applicative[F]): Json => F[Json] = {
      case Json.JArr(vs) =>
        F.fmap(vs.foldLeft(F.pure(Vector.empty[Json]))((acc, v) =>
          F.app(F.fmap(acc, (out: Vector[Json]) => (x: Json) => out :+ x), f(v))), (xs: Vector[Json]) => Json.JArr(xs): Json)
      case other => F.pure(other)
    }
  })

  /** every value of an object, its keys kept */
  val entries: Traversal[Json, Json, Json, Json] = Traversal(new Walk[Json, Json, Json, Json] {
    def apply[F[_]](f: Json => F[Json])(implicit F: Applicative[F]): Json => F[Json] = {
      case Json.JObj(fs) =>
        F.fmap(fs.foldLeft(F.pure(Vector.empty[(String, Json)]))((acc, nv) =>
          F.app(F.fmap(acc, (out: Vector[(String, Json)]) => (x: Json) => out :+ (nv._1 -> x)), f(nv._2))),
          (xs: Vector[(String, Json)]) => Json.JObj(xs): Json)
      case other => F.pure(other)
    }
  })

  // ---------------------------------------------------------------- the dotted path

  /** how many segments a key may have: the descent below is one native
   * frame per segment, and on a recursive schema a key can name a level
   * for every segment it has, so a caller's string is bounded rather
   * than walked (TestJsonOpticDepth) */
  val MaxSegments: Int = 64

  private val here: Affine[Json, Json, Json, Json] = Affine[Json, Json, Json, Json](Right(_), (_, v) => v)

  /**
   * The path a form key names, as an optic, read against the SCHEMA that
   * wrote the Json: `"addr.city"`, `"xs[2]"`, `"$case"`. The schema tells
   * a sum from a product — `{"Case": {...}}` has one more level than the
   * key does. `None` when the key names nothing this schema writes.
   */
  def path(s: Schema[_], key: String): Option[Affine[Json, Json, Json, Json]] = {
    val segs = if (key.isEmpty) Nil else key.split('.').toList
    if (segs.length > MaxSegments) None else go(s, segs, here)
  }

  private def go(sc: Schema[_], segs: List[String], acc: Affine[Json, Json, Json, Json]): Option[Affine[Json, Json, Json, Json]] =
    sc match {
      case i: Schema.SIso[_, _] => go(i.under(), segs, acc)
      case o: Schema.SOption[_] => go(o.of(), segs, acc)
      case su: Schema.SSum[_] => segs match {
        // "$case" addresses the case knob itself: the whole sum object
        case "$case" :: Nil => Some(acc)
        // anything else routes THROUGH a case, a level the key does not mention
        case _ =>
          su.cases.iterator.map { case (n, cs) => go(cs(), segs, seq(acc, caseOf(n))) }
            .collectFirst { case Some(o) => o }
      }
      case _ => segs match {
        case Nil => Some(acc)
        case seg :: rest =>
          val (name, idx) =
            if (seg.endsWith("]") && seg.contains('[')) {
              val at = seg.lastIndexOf('[')
              (seg.take(at), seg.slice(at + 1, seg.length - 1).toIntOption)
            } else (seg, None)
          sc match {
            case p: Schema.SProduct[_] =>
              p.fields.collectFirst { case (n, fs) if n == name => fs() } match {
                case None => None
                case Some(fieldSchema) =>
                  val stepped = seq(acc, field(name))
                  idx match {
                    case None => go(fieldSchema, rest, stepped)
                    case Some(i) => elementOf(fieldSchema) match {
                      case None => None
                      case Some(item) => go(item, rest, seq(stepped, index(i)))
                    }
                  }
              }
            case _ => None
          }
      }
    }

  /** the element schema of a list-shaped node, past the wrappers */
  @annotation.tailrec
  private def elementOf(s: Schema[_]): Option[Schema[_]] = s match {
    case i: Schema.SIso[_, _] => elementOf(i.under())
    case o: Schema.SOption[_] => elementOf(o.of())
    case l: Schema.SList[_] => Some(l.of())
    case v: Schema.SVector[_] => Some(v.of())
    case _ => None
  }

  // ------------------------------------------------------------ the zipper's plate

  /**
   * `Json` as a tree for `Zipper`: an array's children are its values,
   * an object's its field VALUES — the keys stay in the node — and a
   * scalar has none. `withChildren` on an object re-pairs the keys
   * positionally and keeps the node when the arity differs: a key cannot
   * be invented, so a structural edit goes through the parent
   * (`removeChild`/`insertChild`), never through the plate.
   */
  implicit val plate: Plate[Json] = new Plate[Json] {
    def children(t: Json): Vector[Json] = t match {
      case Json.JArr(vs) => vs
      case Json.JObj(fs) => fs.map(_._2)
      case _ => Vector.empty
    }
    def withChildren(t: Json, cs: Vector[Json]): Json = t match {
      case Json.JArr(_) => Json.JArr(cs)
      case Json.JObj(fs) if fs.length == cs.length => Json.JObj(fs.lazyZip(cs).map((f, c) => (f._1, c)))
      case other => other
    }
  }

  /** the i-th child gone — from an array or an object; the identity on a
   * scalar and on an index that is not there */
  def removeChild(j: Json, i: Int): Json = j match {
    case Json.JArr(vs) if vs.isDefinedAt(i) => Json.JArr(vs.patch(i, Nil, 1))
    case Json.JObj(fs) if fs.isDefinedAt(i) => Json.JObj(fs.patch(i, Nil, 1))
    case other => other
  }

  /** `v` inserted at position `i` (0 to the arity, inclusive — at the
   * arity it appends); an object uses `key`, an array ignores it; the
   * identity on a scalar and on a position out of that range */
  def insertChild(j: Json, i: Int, key: String, v: Json): Json = j match {
    case Json.JArr(vs) if i >= 0 && i <= vs.length => Json.JArr(vs.patch(i, Seq(v), 0))
    case Json.JObj(fs) if i >= 0 && i <= fs.length => Json.JObj(fs.patch(i, Seq(key -> v), 0))
    case other => other
  }
}
