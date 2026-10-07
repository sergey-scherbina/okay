package okay.refine

import scala.reflect.ClassTag
import okay.{Prism, Stage}
import okay.freer.{%}
import okay.freer.{!, effect, pure}
import okay.std.{Choose, Throws, choose, raise}
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
 * SERIALIZABLE (refine-bulk): a pattern is a pure value, and a
 * distributed `Bulk` ships it to where the documents are.
 *
 * `<|>` runs every alternative. A choice that stopped at the first
 * taker could never say `Unclear`, and for a document a risk system
 * is about to price, a silent first-wins is the defect this module
 * exists to remove.
 */
sealed trait Refine[A, B] extends Serializable:
  /** the name a verdict's path is written in */
  def name: String

  /** read: the value with the path that produced it, or every reason */
  final def run(a: A): Verdict[B] = Refine.run(this, a, Path.empty)

  /**
   * A PATTERN IS AN EXTRACTOR (refine-match): any pattern is a case of a
   * plain Scala `match`, and a nested pattern is a path —
   * `case Fpml.trade(Fpml.swap(s)) if s.ccy == "EUR" =>`. It matches when
   * the pattern TAKES the input: `Unclear` and `Declined` match no case,
   * so a `match` never takes one reading of an ambiguous document by
   * accident. What a `match` cannot say is WHY a case did not match —
   * for that, `run` (or `Refine.cases`, backlog refine-cases-macro). Each
   * case runs its pattern: put the cheap cases first.
   */
  final def unapply(a: A): Option[B] = run(a).toOption

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

  /** `andThen` in the arrow glyph (Hughes; `Control.Category`) */
  final def >>>[C](next: Refine[B, C]): Refine[A, C] = andThen(next)

  /** a choice: both run, and two takers are `Unclear` */
  final def <|>(alt: Refine[A, B]): Refine[A, B] = (this, alt) match
    case (Refine.Or(xs), Refine.Or(ys)) => Refine.Or(xs ++ ys)
    case (Refine.Or(xs), y) => Refine.Or(xs :+ y)
    case (x, Refine.Or(ys)) => Refine.Or(x +: ys)
    case (x, y) => Refine.Or(Vector(x, y))

  /** `<|>` as a word: EVERY alternative runs, two takers are `Unclear` */
  infix def or(alt: Refine[A, B]): Refine[A, B] = this <|> alt

  /**
   * A FALLBACK, in the sense Scala's `orElse` has everywhere (Option,
   * Either, PartialFunction): `alt` is consulted only when this pattern
   * DECLINES — "the precise reading, else the general one". When this
   * takes (or is `Unclear`), `alt` is not run and cannot make the answer
   * `Unclear`; when it declines, its refusals stay in the verdict, before
   * `alt`'s. This is deliberately NOT `<|>`: a choice that must see both
   * readings is `or`, and the name was kept for the first-wins meaning so
   * nobody reads `orElse` as "every alternative runs" (specs/refine.md,
   * refine-algebra). Write: this pattern's write, else `alt`'s.
   */
  infix def orElse(alt: Refine[A, B]): Refine[A, B] = Refine.OrElse(this, alt)

  /** two patterns side by side on a pair: each reads its own half, each
   * writes its own half. A product's path is the left's, then the
   * right's; two readings on either side are the product of readings */
  final def ***[C, D](that: Refine[C, D]): Refine[(A, C), (B, D)] = Refine.Both(this, that)

  /** two patterns on an `Either`: the Left through this one, the Right
   * through `that`, each case written back through its own side */
  final def +++[C, D](that: Refine[C, D]): Refine[Either[A, C], Either[B, D]] = Refine.Sum(this, that)

  /**
   * Both patterns over the SAME input, both answers as a pair — a
   * record read field by field. The way back writes each half and MERGES
   * the two skeletons (`Refine.Merge`: for `Json`, objects whose fields
   * agree), so `(field("amount") >>> num) and (field("currency") >>> str)`
   * reads `{"amount": 5, "currency": "EUR"}` and writes it back
   * (Rendel and Ostermann's ProductFunctor, for trees rather than
   * strings: invertible-syntax, Haskell Symposium 2010).
   */
  infix def and[C](that: Refine[A, C])(using m: Refine.Merge[A]): Refine[A, (B, C)] = Refine.And(this, that, m)

  /** the read as an EFFECT: the value, or the whole non-`Took` verdict
   * raised through `Throws` — `runEither` hands it back, reasons and all */
  final def orRaise(a: A): B ! Throws % Verdict[B] = run(a) match
    case Verdict.Took(b, _, _) => pure(b)
    case other => raise(other)

  /** the pattern over a STREAM: every input becomes its verdict, one
   * output per input, nothing dropped (okay-stream's `Stage`) */
  final def verdicts: Stage[A, Verdict[B], Unit] =
    Stage.mapAccumulate[A, Verdict[B], Unit](())((u, a) => (u, run(a)))

  /** the pattern over a stream when only the values are wanted: every
   * `Took` value is emitted, and what was NOT taken is not silent — the
   * stage answers how many inputs declined and how many were `Unclear`
   * (`verdicts` keeps their reasons) */
  final def taken: Stage[A, B, Refine.Missed] =
    Stage.transduce[A, B, Refine.Missed](Refine.Missed(0, 0))((m, a) => run(a) match
      case Verdict.Took(b, _, _) => Stage.tell[A, B](b).map(_ => m)
      case Verdict.Unclear(_, _) => pure(m.copy(unclear = m.unclear + 1))
      case Verdict.Declined(_) => pure(m.copy(declined = m.declined + 1)), pure)

  /** routing without channels: one stream out, each element tagged with
   * its route `key(b)` — or `Left` with the input, the verdict and why
   * (an `Unclear` or `Declined` document is never given a key). The
   * synchronous twin of `Router.run`: deterministic, one consumer, every
   * route at one pace */
  final def routed[K](key: B => K): Stage[A, Either[Router.Rejected[A, B], (K, B)], Unit] =
    Stage.mapAccumulate[A, Either[Router.Rejected[A, B], (K, B)], Unit](())((u, a) => (u, run(a) match
      case Verdict.Took(b, _, _) => Right((key(b), b))
      case v @ Verdict.Unclear(cs, _) => Left(Router.Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Router.Rejected(a, v, "declined by every pattern"))))

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
   * A PATH of steps that each keep the type — `path(a, b, c)` is
   * `a >>> b >>> c`, and `path()` is `id`: the category's fold, named
   * (refine-path; the verdict's path is the steps' names in order, no
   * name added). For steps of different types, write the `>>>`s.
   */
  def path[A](steps: Refine[A, A]*): Refine[A, A] =
    steps.foldLeft(id[A])(_ >>> _)

  /**
   * The identity pattern: takes every input and adds NO name to the path,
   * so `id >>> r`, `r >>> id` and `r` answer the same verdict — the
   * category's unit (the laws are TestRefineAlgebra's). A step named
   * "id" would not be one: it would put "id" in every path.
   */
  def id[A]: Refine[A, A] = Id()

  /** the choice of no alternatives: declines everything with no reason,
   * writes nothing — the unit of `<|>`/`or` and of `orElse` */
  def empty[A, B]: Refine[A, B] = Or(Vector.empty)

  /** a pattern's patterns compose: `id` and `>>>` (okay-optics' `Category`) */
  given category: okay.Optic.Category[Refine] with
    def id[A]: Refine[A, A] = Refine.id[A]
    def compose[A, B, C](g: Refine[B, C], f: Refine[A, B]): Refine[A, C] = f >>> g

  /** how `and` puts two written skeletons back into one input */
  trait Merge[A] extends Serializable:
    def merge(x: A, y: A): Either[String, A]

  object Merge:
    /** two JSON objects become one; a field both write must agree; a
     * non-object is merged only with an equal value */
    given Merge[Json] with
      def merge(x: Json, y: Json): Either[String, Json] = (x, y) match
        case (Json.JObj(xs), Json.JObj(ys)) =>
          val clash = ys.collectFirst { case (k, v) if xs.exists((k2, v2) => k2 == k && v2 != v) => k }
          clash match
            case Some(k) => Left(s"both halves write field `$k`, differently")
            case None => Right(Json.JObj(xs ++ ys.filterNot((k, _) => xs.exists(_._1 == k))))
        case _ if x == y => Right(x)
        case _ => Left(s"cannot merge ${Json.print(x).take(40)} with ${Json.print(y).take(40)}")

  /** what `taken` did not emit: inputs that declined, inputs that were `Unclear` */
  final case class Missed(declined: Int, unclear: Int)

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

    /** a descent through fields: `at("dataDocument", "trade", "swap")`,
     * each field a step of the path, named as the field */
    def at(names: String*): Refine[Json, Json] = path(names.map(field)*)

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
    def name: String = first match
      case Id() => second.name
      case _ => first.name

  final case class Id[A]() extends Refine[A, A]:
    def name: String = "id"

  final case class OrElse[A, B](first: Refine[A, B], second: Refine[A, B]) extends Refine[A, B]:
    def name: String = s"${first.name} orElse ${second.name}"

  final case class Both[A, B, C, D](left: Refine[A, B], right: Refine[C, D]) extends Refine[(A, C), (B, D)]:
    def name: String = s"${left.name}*${right.name}"

  final case class Sum[A, B, C, D](left: Refine[A, B], right: Refine[C, D]) extends Refine[Either[A, C], Either[B, D]]:
    def name: String = s"${left.name}+${right.name}"

  final case class And[A, B, C](left: Refine[A, B], right: Refine[A, C], merge: Merge[A]) extends Refine[A, (B, C)]:
    def name: String = s"${left.name}&${right.name}"

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
    case Id() => Verdict.Took(a, at, Vector.empty)
    case OrElse(f, s) => run(f, a, at) match
      case Verdict.Declined(d) => run(s, a, at) match
        case Verdict.Took(b, by, d2) => Verdict.Took(b, by, d ++ d2)
        case Verdict.Unclear(cs, d2) => Verdict.Unclear(cs, d ++ d2)
        case Verdict.Declined(d2) => Verdict.Declined(d ++ d2)
      case taken => taken
    case Both(l, r) => product(run(l, a._1, at), run(r, a._2, at), at)
    case Sum(l, r) => a match
      case Left(x) => run(l, x, at) match
        case Verdict.Took(b, by, d) => Verdict.Took(Left(b), by, d)
        case Verdict.Unclear(cs, d) => Verdict.Unclear(cs.map((by, b) => (by, Left(b))), d)
        case Verdict.Declined(d) => Verdict.Declined(d)
      case Right(y) => run(r, y, at) match
        case Verdict.Took(b, by, d) => Verdict.Took(Right(b), by, d)
        case Verdict.Unclear(cs, d) => Verdict.Unclear(cs.map((by, b) => (by, Right(b))), d)
        case Verdict.Declined(d) => Verdict.Declined(d)
    case And(l, r, _) => product(run(l, a, at), run(r, a, at), at)

  /** two verdicts side by side: every pairing of their readings; the
   * pair's path is the left's, then the right's steps below `at` */
  private def product[B, C](x: Verdict[B], y: Verdict[C], at: Path): Verdict[(B, C)] =
    def readings[T](v: Verdict[T]): Vector[(Path, T)] = v match
      case Verdict.Took(t, by, _) => Vector((by, t))
      case Verdict.Unclear(cs, _) => cs
      case Verdict.Declined(_) => Vector.empty
    val pairs = for (p1, b) <- readings(x); (p2, c) <- readings(y) yield (Path(p1.steps ++ p2.steps.drop(at.steps.length)), (b, c))
    val declined = x.reasons ++ y.reasons
    pairs match
      case Vector((by, bc)) => Verdict.Took(bc, by, declined)
      case Vector() => Verdict.Declined(declined)
      case many => Verdict.Unclear(many, declined)

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
    case Id() => Right(b)
    case OrElse(f, s) => write(f, b) match
      case Left(_) => write(s, b)
      case ok => ok
    case Both(l, r) => for x <- write(l, b._1); y <- write(r, b._2) yield (x, y)
    case Sum(l, r) => b match
      case Left(x) => write(l, x).map(Left(_))
      case Right(y) => write(r, y).map(Right(_))
    case And(l, r, m) => for x <- write(l, b._1); y <- write(r, b._2); xy <- m.merge(x, y) yield xy
