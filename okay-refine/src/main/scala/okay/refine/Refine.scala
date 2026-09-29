package okay.refine

import scala.reflect.ClassTag
import okay.{!, Choose, Prism, choose, effect, pure}
import okay.codec.{Json, Schema}

/**
 * A PATTERN: from what is known about a document (`A`) to what is
 * learnt (`B`), or a refusal that says why — and the way back
 * (specs/refine.md).
 *
 * A step is a prism given as its two halves, `read` partial and
 * `write` total; `andThen` is a path through the hierarchy, `<|>` a
 * choice at one level. The tree stays a tree, so a run walks it and
 * records WHICH names took the input and which declined: the answer
 * is a `Verdict`, never a bare value, because a hierarchy's whole
 * worth over a hand-written `match` is that the reader sees what was
 * considered.
 *
 * `<|>` runs every alternative. A choice that stopped at the first
 * taker could never say `Unclear`, and for a document a risk system
 * is about to price, a silent first-wins is the defect this module
 * exists to remove.
 */
sealed trait Refine[A, B]:
  /** the name a verdict's path is written in */
  def name: String

  /** read: the value with the path that produced it, or every reason */
  final def run(a: A): Verdict[B] = Refine.run(this, a, Path.empty)

  /** the way back — total on what any branch of this pattern produced,
   * `Left` for a value no branch would have (specs/refine.md, Decisions) */
  final def write(b: B): Either[String, A] = Refine.write(this, b)

  /**
   * The pattern as a SEARCH (specs/refine.md, stage 2; pattern-binds):
   * a taker is the answer, `Unclear` is a choice point over its
   * candidates, `Declined` kills the branch — so `runChoice` lists the
   * readings, `Logic.ifte` writes "if this is a swap then … else …"
   * without losing the others, and `for case` prunes. The reasons are
   * the verdict's; a search that needs them runs `run`.
   */
  final def search(a: A): B ! Choose = run(a) match
    case Verdict.Took(b, _, _) => pure(b)
    case Verdict.Unclear(cs, _) => choose(cs.map(_._2)*)
    case Verdict.Declined(_) => effect(Choose(Seq.empty))

  /** a path: the second pattern over what the first learnt */
  infix def andThen[C](next: Refine[B, C]): Refine[A, C] = Refine.AndThen(this, next)

  /** a choice: both run, and two takers are `Unclear` */
  final def <|>(alt: Refine[A, B]): Refine[A, B] = (this, alt) match
    case (Refine.Or(xs), Refine.Or(ys)) => Refine.Or(xs ++ ys)
    case (Refine.Or(xs), y) => Refine.Or(xs :+ y)
    case (x, Refine.Or(ys)) => Refine.Or(x +: ys)
    case (x, y) => Refine.Or(Vector(x, y))

  /** an iso on what is learnt, both ways, so the path still writes */
  final def map[C](name: String)(to: B => C, from: C => B): Refine[A, C] =
    Refine.Map(this, name, to, c => Right(from(c)))

  /** into a sum: what this learns is one case of `C`, and `write` takes
   * that case only — the type test is the one cast this module makes,
   * isolated here (specs/refine.md, "write is partial on a sum") */
  final def widen[C >: B](using ct: ClassTag[B]): Refine[A, C] =
    Refine.Map[A, B, C](this, name, identity, c => ct.unapply(c).toRight(s"$name: not this case"))

object Refine:

  /** a step: the prism's two halves, under a name */
  def step[A, B](name: String)(read: A => Either[String, B])(write: B => A): Refine[A, B] =
    Step(name, read, write)

  /**
   * A derived `Schema` IS a pattern (specs/refine.md, stage 2): the
   * codec's decode is the read, declining in the codec's own words, and
   * its encode the write. The reified type is data, so a new instrument
   * is a new Schema and a registration, and nothing existing is edited.
   * The write goes through the text encoder and back to a value — the
   * codec has no value-level `A => Json`; one round trip per write,
   * priced when a consumer shows it in a profile.
   */
  def schema[A](name: String)(using s: Schema[A]): Refine[Json, A] =
    Step(name, j => Json.decode(s)(j), a => Json.parse(Json.encode(s)(a)))

  /** the `<|>` of many */
  def first[A, B](alts: Refine[A, B]*): Refine[A, B] =
    alts.reduceLeft(_ <|> _)

  /**
   * Patterns over the one `Json` value every dialect projects into
   * (refine-fpml-prover): a field, a string, a number, every element of
   * a repeated field. Each is a step, so a path of them names itself in
   * the verdict (`trade/swap/swapStream`) and writes back a skeleton.
   */
  object json:
    /** the field, or "no field `name`" — writes a one-field object */
    def field(name: String): Refine[Json, Json] =
      step[Json, Json](name) {
        case Json.JObj(fs) => fs.collectFirst { case (n, v) if n == name => v }.toRight(s"no field `$name`")
        case other => Left(s"not an object: ${other.getClass.getSimpleName}")
      }(v => Json.JObj(Vector(name -> v)))

    /** the string a field holds */
    val str: Refine[Json, String] =
      step[Json, String]("string") {
        case Json.JStr(s) => Right(s)
        case other => Left(s"not a string: ${Json.print(other)}")
      }(Json.JStr(_))

    /** a number, or a string that reads as one — XML text is text */
    val num: Refine[Json, Double] =
      step[Json, Double]("number") {
        case Json.JNum(n) => Right(n)
        case Json.JStr(s) => s.trim.toDoubleOption.toRight(s"not a number: '$s'")
        case other => Left(s"not a number: ${Json.print(other)}")
      }(Json.JNum(_))

    /** a field's elements: an array's, or the one value alone — an XML
     * child that happens to occur once is not an array */
    def each(name: String): Refine[Json, Vector[Json]] =
      field(name).map("each")(
        { case Json.JArr(vs) => vs; case one => Vector(one) },
        { case Vector(one) => one; case vs => Json.JArr(vs) })

  final case class Step[A, B](name: String, read: A => Either[String, B], back: B => A) extends Refine[A, B]:
    /** the step as the optics prism it is, so specs/optics.md's laws apply */
    def prism: Prism[A, A, B, B] = Prism(a => read(a).left.map(_ => a), back)

  final case class AndThen[A, X, B](first: Refine[A, X], second: Refine[X, B]) extends Refine[A, B]:
    def name: String = first.name

  final case class Or[A, B](alts: Vector[Refine[A, B]]) extends Refine[A, B]:
    def name: String = alts.map(_.name).mkString("|")

  /** `from` may refuse: a `widen` handed a case of the sum that is not
   * this branch's says so, and `Or.write` asks the next alternative */
  final case class Map[A, X, B](under: Refine[A, X], name: String, to: X => B, from: B => Either[String, X]) extends Refine[A, B]

  // BOUNDED: per level of the pattern tree the program built — the
  // nesting of `andThen`/`<|>`/`map` in the authoring source, which a
  // registry only widens (one `Or` of many is one level), never deepens
  private def run[A, B](r: Refine[A, B], a: A, at: Path): Verdict[B] = r match
    case Step(name, read, _) => read(a) match
      case Right(b) => Verdict.Took(b, at / name, Vector.empty)
      case Left(why) => Verdict.Declined(Vector(Refusal(at / name, why)))
    case AndThen(f, s) => run(f, a, at) match
      case Verdict.Took(x, by, declined) => run(s, x, by) match
        case Verdict.Took(b, by2, d2) => Verdict.Took(b, by2, declined ++ d2)
        case Verdict.Unclear(cs, d2) => Verdict.Unclear(cs, declined ++ d2)
        case Verdict.Declined(d2) => Verdict.Declined(declined ++ d2)
      case Verdict.Unclear(cs, declined) =>
        // every candidate of the first goes on through the second
        val outs = cs.map((by, x) => run(s, x, by))
        val took = outs.collect { case Verdict.Took(b, by, _) => (by, b) } ++
          outs.collect { case Verdict.Unclear(cs2, _) => cs2 }.flatten
        val d2 = declined ++ outs.flatMap {
          case Verdict.Took(_, _, d) => d
          case Verdict.Unclear(_, d) => d
          case Verdict.Declined(d) => d
        }
        took match
          case Vector((by, b)) => Verdict.Took(b, by, d2)
          case Vector() => Verdict.Declined(d2)
          case many => Verdict.Unclear(many, d2)
      case Verdict.Declined(d) => Verdict.Declined(d)
    case Or(alts) =>
      val outs = alts.map(run(_, a, at))
      val took = outs.collect { case Verdict.Took(b, by, _) => (by, b) } ++
        outs.collect { case Verdict.Unclear(cs, _) => cs }.flatten
      val declined = outs.flatMap {
        case Verdict.Took(_, _, d) => d
        case Verdict.Unclear(_, d) => d
        case Verdict.Declined(d) => d
      }
      took match
        case Vector((by, b)) => Verdict.Took(b, by, declined)
        case Vector() => Verdict.Declined(declined)
        case many => Verdict.Unclear(many, declined)
    case Map(under, _, to, _) => run(under, a, at) match
      case Verdict.Took(x, by, d) => Verdict.Took(to(x), by, d)
      case Verdict.Unclear(cs, d) => Verdict.Unclear(cs.map((by, x) => (by, to(x))), d)
      case Verdict.Declined(d) => Verdict.Declined(d)

  // BOUNDED: as `run` above, per level of the authored pattern tree
  private def write[A, B](r: Refine[A, B], b: B): Either[String, A] = r match
    case Step(_, _, back) => Right(back(b))
    case AndThen(f, s) => write(s, b).flatMap(write(f, _))
    case Or(alts) =>
      // the alternatives in order, the first whose branch this value is
      var i = 0
      var out: Either[String, A] = Left(s"${r.name}: no alternative writes this value")
      while out.isLeft && i < alts.length do
        write(alts(i), b) match
          case Right(a) => out = Right(a)
          case Left(_) => ()
        i += 1
      out
    case Map(under, _, _, from) => from(b).flatMap(write(under, _))
