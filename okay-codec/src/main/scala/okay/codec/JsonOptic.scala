package okay.codec

import okay.*

/**
 * Optics over `Json` (specs/optics.md stage 1). They are the other
 * carrier: for a derived `Schema[A]`, a field's lens on the VALUE and
 * that field's optic on its JSON commute with the codec, which is the
 * drift law of the second order — the form and the parser could not
 * drift (ui-toolkit's law), and now an EDIT to the value and the same
 * edit to its wire shape cannot either.
 *
 * The lawful one is `at`, whose focus is an `Option[Json]`: absent is
 * `None`, `set(None)` removes, `set(Some(v))` inserts or replaces. The
 * ones a path is written with — `field`, `index`, `caseOf` — are
 * affines derived from it (or built directly), which is the `at`/`ix`
 * pair every optics library ends up with, for the reason this one did:
 * a lens that CREATES a missing field breaks GetPut, and a library
 * whose tests are laws cannot ship that quietly.
 *
 * Totality: applied to a Json of the wrong shape (a field optic on a
 * number, an index on an object) every one of these is the identity
 * and previews nothing. That keeps them total, and it is why the laws
 * below are stated over objects and arrays rather than over all Json.
 *
 * ONE LAW HOLDS MODULO FIELD ORDER, and the test says so rather than
 * pretending otherwise: `at`'s PutPut — `set(w) ∘ set(v) == set(w)` —
 * is exact except when `v` is `None`, because a removal loses where
 * the field was and the next insert appends. JSON objects are
 * unordered by RFC 8259, `JObj` keeps a Vector because the CODEC's
 * field order is worth preserving, and those two facts meet here.
 * Every other law, and the drift law itself, is exact.
 */
object JsonOptic {

  /** the field as an `Option`: THE lawful lens — absent is None,
   * `set(None)` removes, `set(Some(v))` inserts or replaces */
  def at(name: String): Lens[Json, Json, Option[Json], Option[Json]] =
    Lens(
      {
        case Json.JObj(fs) => fs.collectFirst { case (n, v) if n == name => v }
        case _ => None
      },
      (j, ov) => j match
        case Json.JObj(fs) => ov match
          case Some(v) =>
            if fs.exists(_._1 == name) then Json.JObj(fs.map((n, old) => if n == name then (n, v) else (n, old)))
            else Json.JObj(fs :+ (name -> v))
          case None => Json.JObj(fs.filterNot(_._1 == name))
        case other => other)

  /** the field when it is there: `at(name)` through `Some` */
  def field(name: String): Affine[Json, Json, Json, Json] = at(name).andThen(Prism.some)

  /** the i-th element of an array, when the array has one */
  def index(i: Int): Affine[Json, Json, Json, Json] =
    Affine(
      {
        case Json.JArr(vs) if i >= 0 && i < vs.length => Right(vs(i))
        case other => Left(other)
      },
      (j, v) => j match
        case Json.JArr(vs) if i >= 0 && i < vs.length => Json.JArr(vs.updated(i, v))
        case other => other)

  /** the codec's sum shape, `{"Case": value}`, when the case is this one */
  def caseOf(name: String): Affine[Json, Json, Json, Json] =
    Affine(
      {
        case Json.JObj(Vector((n, v))) if n == name => Right(v)
        case other => Left(other)
      },
      (j, v) => j match
        case Json.JObj(Vector((n, _))) if n == name => Json.JObj(Vector(n -> v))
        case other => other)

  /** every element of an array, in order */
  def values: Traversal[Json, Json, Json, Json] =
    Traversal([F[_]] => (F: Applicative[F]) ?=> (f: Json => F[Json]) => (j: Json) =>
      j match
        case Json.JArr(vs) =>
          F.fmap(vs.foldLeft(F.pure(Vector.empty[Json]))((acc, v) =>
            F.fmap(acc, (out: Vector[Json]) => (x: Json) => out :+ x).app(f(v))), Json.JArr(_))
        case other => F.pure(other))

  /** every value of an object, its keys kept */
  def entries: Traversal[Json, Json, Json, Json] =
    Traversal([F[_]] => (F: Applicative[F]) ?=> (f: Json => F[Json]) => (j: Json) =>
      j match
        case Json.JObj(fs) =>
          F.fmap(fs.foldLeft(F.pure(Vector.empty[(String, Json)]))((acc, nv) =>
            F.fmap(acc, (out: Vector[(String, Json)]) => (x: Json) => out :+ (nv._1 -> x)).app(f(nv._2))),
            Json.JObj(_))
        case other => F.pure(other))

  // ---------------------------------------------------------------- the dotted path

  /**
   * The path a form key names, as an optic, read against the SCHEMA
   * that wrote the Json: `"addr.city"`, `"xs[2]"`, `"$case"`. The
   * schema is what tells a sum from a product — `{"Case": {...}}` has
   * one more level than the key does, and only the schema knows it is
   * there. Answers `None` when the key names nothing this schema
   * writes, which is the same refusal `Form.edit` makes by returning
   * the value unchanged.
   */
  def path(s: Schema[?], key: String): Option[Affine[Json, Json, Json, Json]] =
    val identity: Affine[Json, Json, Json, Json] = Affine(Right(_), (_, v) => v)
    def segments(k: String): List[String] = if k.isEmpty then Nil else k.split('.').toList

    def go(sc: Schema[?], segs: List[String], acc: Affine[Json, Json, Json, Json]): Option[Affine[Json, Json, Json, Json]] =
      sc match
        case Schema.SIso(u, _, _) => go(u(), segs, acc)
        case Schema.SOption(of) => go(of(), segs, acc)
        case su: Schema.SSum[?] => segs match
          // "$case" addresses the case knob itself: the whole sum object
          case "$case" :: Nil => Some(acc)
          // anything else routes THROUGH a case, and the schema knows
          // the level the key does not mention
          case _ =>
            su.cases.iterator.map((n, cs) => go(cs(), segs, acc.andThen(caseOf(n))))
              .collectFirst { case Some(o) => o }
        case _ => segs match
          case Nil => Some(acc)
          case seg :: rest =>
            val (name, idx) =
              if seg.endsWith("]") && seg.contains('[') then
                val at = seg.lastIndexOf('[')
                (seg.take(at), seg.slice(at + 1, seg.length - 1).toIntOption)
              else (seg, None)
            sc match
              case p: Schema.SProduct[?] =>
                p.fields.collectFirst { case (n, fs) if n == name => fs() } match
                  case None => None
                  case Some(fieldSchema) =>
                    val stepped = acc.andThen(field(name))
                    idx match
                      case None => go(fieldSchema, rest, stepped)
                      case Some(i) => elementOf(fieldSchema) match
                        case None => None
                        case Some(item) => go(item, rest, stepped.andThen(index(i)))
              case _ => None

    go(s, segments(key), identity)

  /** the element schema of a list-shaped node, past the wrappers */
  private def elementOf(s: Schema[?]): Option[Schema[?]] = s match
    case Schema.SIso(u, _, _) => elementOf(u())
    case Schema.SOption(of) => elementOf(of())
    case Schema.SList(of) => Some(of())
    case Schema.SVector(of) => Some(of())
    case _ => None
}
