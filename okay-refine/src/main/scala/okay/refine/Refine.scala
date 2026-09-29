package okay.refine

import scala.reflect.ClassTag
import okay.Prism

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

  /** the `<|>` of many */
  def first[A, B](alts: Refine[A, B]*): Refine[A, B] =
    alts.reduceLeft(_ <|> _)

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
