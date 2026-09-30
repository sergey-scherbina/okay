package okay2.refine

import scala.reflect.ClassTag
import okay2.{!, Choose, Optic, Throws, pure}
import okay2.Optic.Prism
import okay2.stream.Stage
import okay2.codec.{Json, Schema}

/**
 * A PATTERN: from what is known about a document (`A`) to what is
 * learnt (`B`), or a refusal that says why — and the way back. The
 * Scala 2.13 port of okay-refine (okay's specs/refine.md); the shape
 * is the same, the dispatch is the trait's own methods rather than a
 * match over the tree (Scala 2 does not refine the existential of a
 * generic case class across a match the way Scala 3's GADT check does).
 *
 * A step is a prism given as its two halves, `read` partial and
 * `write` total; `andThen` is a path through the hierarchy, `<|>` a
 * choice at one level. The tree stays a tree, so a run walks it and
 * records WHICH names took the input and which declined: the answer
 * is a `Verdict`, never a bare value.
 *
 * `<|>` runs every alternative. A choice that stopped at the first
 * taker could never say `Unclear`.
 */
sealed trait Refine[A, B] extends Serializable {
  /** the name a verdict's path is written in */
  def name: String

  /** read: the value with the path that produced it, or every reason */
  final def run(a: A): Verdict[B] = runAt(a, Path.empty)

  /** the way back — total on what any branch of this pattern produced,
   * `Left` for a value no branch would have */
  final def write(b: B): Either[String, A] = writeBack(b)

  /**
   * The pattern as a SEARCH: a taker is the answer, `Unclear` is a
   * choice point over its candidates, `Declined` kills the branch — so
   * `runChoice` lists the readings and `Logic.ifte` writes "if this is
   * a swap then … else …" without losing the others.
   */
  final def search(a: A): B ! Choose = run(a) match {
    case Verdict.Took(b, _, _) => pure[Choose, B](b)
    case Verdict.Unclear(cs, _) => Choose.choose(cs.map(_._2): _*)
    case Verdict.Declined(_) => Choose.fail[B]
  }

  /** a path: the second pattern over what the first learnt */
  def andThen[C](next: Refine[B, C]): Refine[A, C] = Refine.AndThen(this, next)

  /** `andThen` in the arrow glyph (Hughes; `Control.Category`) */
  final def >>>[C](next: Refine[B, C]): Refine[A, C] = andThen(next)

  /** a choice: both run, and two takers are `Unclear` */
  final def <|>(alt: Refine[A, B]): Refine[A, B] = (this, alt) match {
    case (Refine.Or(xs), Refine.Or(ys)) => Refine.Or(xs ++ ys)
    case (Refine.Or(xs), y) => Refine.Or(xs :+ y)
    case (x, Refine.Or(ys)) => Refine.Or(x +: ys)
    case (x, y) => Refine.Or(Vector(x, y))
  }

  /** `<|>` as a word: EVERY alternative runs, two takers are `Unclear` */
  final def or(alt: Refine[A, B]): Refine[A, B] = this <|> alt

  /**
   * A FALLBACK, in the sense Scala's `orElse` has everywhere (Option,
   * Either, PartialFunction): `alt` is consulted only when this pattern
   * DECLINES. When this takes (or is `Unclear`), `alt` is not run and
   * cannot make the answer `Unclear`; when it declines, its refusals stay
   * in the verdict, before `alt`'s. Deliberately NOT `<|>` — that is `or`
   * (okay's specs/refine.md, refine-algebra). Write: this pattern's,
   * else `alt`'s.
   */
  final def orElse(alt: Refine[A, B]): Refine[A, B] = Refine.OrElse(this, alt)

  /** two patterns side by side on a pair; the path is the left's, then the right's */
  final def ***[C, D](that: Refine[C, D]): Refine[(A, C), (B, D)] = Refine.Both(this, that)

  /** two patterns on an `Either`, each case through its own side */
  final def +++[C, D](that: Refine[C, D]): Refine[Either[A, C], Either[B, D]] = Refine.Sum(this, that)

  /** both patterns over the SAME input, both answers as a pair — a record
   * read field by field; the way back MERGES the two written skeletons
   * (`Refine.Merge`: for `Json`, objects whose fields agree) */
  final def and[C](that: Refine[A, C])(implicit m: Refine.Merge[A]): Refine[A, (B, C)] = Refine.And(this, that, m)

  /** the read as an EFFECT: the value, or the whole non-`Took` verdict
   * raised through `Throws` — `Throws.runEither` hands it back */
  final def orRaise(a: A): B ! Throws[Verdict[B]] = run(a) match {
    case Verdict.Took(b, _, _) => pure[Throws[Verdict[B]], B](b)
    case other => Throws.raise[Verdict[B], B](other)
  }

  /** the pattern over a STREAM: every input becomes its verdict, nothing dropped */
  final def verdicts: okay2.stream.Stage[A, Verdict[B], Unit] =
    Stage.mapAccumulate[A, Verdict[B], Unit](())((u, a) => (u, run(a)))

  /** the values only — and what was NOT taken is not silent: the stage
   * answers how many inputs declined and how many were `Unclear` */
  final def taken: okay2.stream.Stage[A, B, Refine.Missed] =
    Stage.transduce[A, B, Refine.Missed](Refine.Missed(0, 0))((m, a) => run(a) match {
      case Verdict.Took(b, _, _) => Stage.tell[A, B](b).map(_ => m)
      case Verdict.Unclear(_, _) => pure[okay2.stream.Take[A] with okay2.Writer[B], Refine.Missed](m.copy(unclear = m.unclear + 1))
      case Verdict.Declined(_) => pure[okay2.stream.Take[A] with okay2.Writer[B], Refine.Missed](m.copy(declined = m.declined + 1))
    }, m => pure[okay2.stream.Take[A] with okay2.Writer[B], Refine.Missed](m))

  /** routing without channels: one stream out, each element tagged with
   * its route `key(b)` — or `Left` with the input, the verdict and why
   * (an `Unclear` or `Declined` document is never given a key); the
   * synchronous twin of `Router.run` */
  final def routed[K](key: B => K): okay2.stream.Stage[A, Either[Router.Rejected[A, B], (K, B)], Unit] =
    Stage.mapAccumulate[A, Either[Router.Rejected[A, B], (K, B)], Unit](())((u, a) => (u, run(a) match {
      case Verdict.Took(b, _, _) => Right((key(b), b))
      case v @ Verdict.Unclear(cs, _) => Left(Router.Rejected(a, v, s"unclear: ${cs.map(_._1).mkString(" | ")}"))
      case v @ Verdict.Declined(_) => Left(Router.Rejected(a, v, "declined by every pattern"))
    }))

  /** an iso on what is learnt, both ways, so the path still writes */
  final def map[C](name: String)(to: B => C, from: C => B): Refine[A, C] =
    Refine.Map[A, B, C](this, name, to, c => Right(from(c)))

  /** into a sum: what this learns is one case of `C`, and `write` takes
   * that case only — the type test is the one cast this module makes */
  final def widen[C >: B](implicit ct: ClassTag[B]): Refine[A, C] =
    Refine.Map[A, B, C](this, name, identity, c => ct.unapply(c).toRight(s"$name: not this case"))

  protected[refine] def runAt(a: A, at: Path): Verdict[B]
  protected[refine] def writeBack(b: B): Either[String, A]
}

object Refine {

  /** a step: the prism's two halves, under a name */
  def step[A, B](name: String)(read: A => Either[String, B])(write: B => A): Refine[A, B] =
    Step(name, read, write)

  /**
   * A derived `Schema` IS a pattern: the codec's decode is the read,
   * declining in the codec's own words, and its encode the write. The
   * write goes through the text encoder and back to a value — one round
   * trip per write, as in okay-refine.
   */
  def schema[A](name: String)(implicit s: Schema[A]): Refine[Json, A] =
    Step[Json, A](name, j => Json.decode(s)(j), a => Json.parse(Json.encode(s)(a)))

  /** the `<|>` of many */
  def first[A, B](alts: Refine[A, B]*): Refine[A, B] =
    alts.reduceLeft(_ <|> _)

  /** the identity pattern: takes every input and adds NO name to the
   * path, so `id >>> r`, `r >>> id` and `r` answer the same verdict */
  def id[A]: Refine[A, A] = Id[A]()

  /** the choice of no alternatives: the unit of `or` and of `orElse` */
  def empty[A, B]: Refine[A, B] = Or[A, B](Vector.empty)

  /** a pattern's patterns compose: `id` and `>>>` (okay2-optics' `Category`) */
  implicit val category: Optic.Category[Refine] = new Optic.Category[Refine] {
    def id[A]: Refine[A, A] = Refine.id[A]
    def compose[A, B, C](g: Refine[B, C], f: Refine[A, B]): Refine[A, C] = f >>> g
  }

  /** how `and` puts two written skeletons back into one input */
  trait Merge[A] extends Serializable {
    def merge(x: A, y: A): Either[String, A]
  }

  object Merge {
    /** two JSON objects become one; a field both write must agree; a
     * non-object is merged only with an equal value */
    implicit val json: Merge[Json] = new Merge[Json] {
      def merge(x: Json, y: Json): Either[String, Json] = (x, y) match {
        case (Json.JObj(xs), Json.JObj(ys)) =>
          ys.collectFirst { case (k, v) if xs.exists { case (k2, v2) => k2 == k && v2 != v } => k } match {
            case Some(k) => Left(s"both halves write field `$k`, differently")
            case None => Right(Json.JObj(xs ++ ys.filterNot { case (k, _) => xs.exists(_._1 == k) }))
          }
        case _ if x == y => Right(x)
        case _ => Left(s"cannot merge ${Json.print(x).take(40)} with ${Json.print(y).take(40)}")
      }
    }
  }

  /** what `taken` did not emit: inputs that declined, inputs that were `Unclear` */
  final case class Missed(declined: Int, unclear: Int)

  /** two verdicts side by side: every pairing of their readings; the
   * pair's path is the left's, then the right's steps below `at` */
  private def product[B, C](x: Verdict[B], y: Verdict[C], at: Path): Verdict[(B, C)] = {
    def readings[T](v: Verdict[T]): Vector[(Path, T)] = v match {
      case Verdict.Took(t, by, _) => Vector((by, t))
      case Verdict.Unclear(cs, _) => cs
      case Verdict.Declined(_) => Vector.empty
    }
    val pairs = for ((p1, b) <- readings(x); (p2, c) <- readings(y)) yield (Path(p1.steps ++ p2.steps.drop(at.steps.length)), (b, c))
    val declined = x.reasons ++ y.reasons
    pairs match {
      case Vector((by, bc)) => Verdict.Took(bc, by, declined)
      case Vector() => Verdict.Declined(declined)
      case many => Verdict.Unclear(many, declined)
    }
  }

  /**
   * Patterns over the one `Json` value every dialect projects into: a
   * field, a string, a number, every element of a repeated field. Each
   * is a step, so a path of them names itself in the verdict and writes
   * back a skeleton.
   */
  object json {
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
  }

  /** the outcome of running every alternative of one level: one taker is
   * `Took`, several are `Unclear`, none is `Declined` — with every reason */
  private def level[B](outs: Vector[Verdict[B]], before: Vector[Refusal]): Verdict[B] = {
    val took = outs.collect { case Verdict.Took(b, by, _) => (by, b) } ++
      outs.collect { case Verdict.Unclear(cs, _) => cs }.flatten
    val declined = before ++ outs.flatMap(_.reasons)
    took match {
      case Vector((by, b)) => Verdict.Took(b, by, declined)
      case Vector() => Verdict.Declined(declined)
      case many => Verdict.Unclear(many, declined)
    }
  }

  final case class Step[A, B](name: String, read: A => Either[String, B], back: B => A) extends Refine[A, B] {
    /** the step as the optics prism it is, so the prism laws apply */
    def prism: Prism[A, A, B, B] = Prism[A, A, B, B](a => read(a).left.map(_ => a), back)

    protected[refine] def runAt(a: A, at: Path): Verdict[B] = read(a) match {
      case Right(b) => Verdict.Took(b, at / name, Vector.empty)
      case Left(why) => Verdict.Declined(Vector(Refusal(at / name, why)))
    }
    protected[refine] def writeBack(b: B): Either[String, A] = Right(back(b))
  }

  // BOUNDED: the recursion of runAt/writeBack through AndThen, Or and
  // Map is per level of the pattern tree the program built — the
  // nesting of `andThen`/`<|>`/`map` in the authoring source, which a
  // registry only widens (one `Or` of many is one level), never deepens
  final case class AndThen[A, X, B](first: Refine[A, X], second: Refine[X, B]) extends Refine[A, B] {
    def name: String = first match {
      case _: Id[_] => second.name
      case _ => first.name
    }

    protected[refine] def runAt(a: A, at: Path): Verdict[B] = first.runAt(a, at) match {
      case Verdict.Took(x, by, declined) => second.runAt(x, by) match {
        case Verdict.Took(b, by2, d2) => Verdict.Took(b, by2, declined ++ d2)
        case Verdict.Unclear(cs, d2) => Verdict.Unclear(cs, declined ++ d2)
        case Verdict.Declined(d2) => Verdict.Declined(declined ++ d2)
      }
      // every candidate of the first goes on through the second
      case Verdict.Unclear(cs, declined) => level(cs.map { case (by, x) => second.runAt(x, by) }, declined)
      case Verdict.Declined(d) => Verdict.Declined(d)
    }
    protected[refine] def writeBack(b: B): Either[String, A] = second.writeBack(b).flatMap(first.writeBack)
  }

  final case class Or[A, B](alts: Vector[Refine[A, B]]) extends Refine[A, B] {
    def name: String = alts.map(_.name).mkString("|")

    protected[refine] def runAt(a: A, at: Path): Verdict[B] = level(alts.map(_.runAt(a, at)), Vector.empty)

    /** the alternatives in order, the first whose branch this value is */
    protected[refine] def writeBack(b: B): Either[String, A] = {
      var i = 0
      var out: Either[String, A] = Left(s"$name: no alternative writes this value")
      while (out.isLeft && i < alts.length) {
        alts(i).writeBack(b) match {
          case Right(a) => out = Right(a)
          case Left(_) => ()
        }
        i += 1
      }
      out
    }
  }

  /** `from` may refuse: a `widen` handed a case of the sum that is not
   * this branch's says so, and `Or.writeBack` asks the next alternative */
  final case class Map[A, X, B](under: Refine[A, X], name: String, to: X => B, from: B => Either[String, X]) extends Refine[A, B] {
    protected[refine] def runAt(a: A, at: Path): Verdict[B] = under.runAt(a, at) match {
      case Verdict.Took(x, by, d) => Verdict.Took(to(x), by, d)
      case Verdict.Unclear(cs, d) => Verdict.Unclear(cs.map { case (by, x) => (by, to(x)) }, d)
      case Verdict.Declined(d) => Verdict.Declined(d)
    }
    protected[refine] def writeBack(b: B): Either[String, A] = from(b).flatMap(under.writeBack)
  }

  final case class Id[A]() extends Refine[A, A] {
    def name: String = "id"
    protected[refine] def runAt(a: A, at: Path): Verdict[A] = Verdict.Took(a, at, Vector.empty)
    protected[refine] def writeBack(b: A): Either[String, A] = Right(b)
  }

  final case class OrElse[A, B](first: Refine[A, B], second: Refine[A, B]) extends Refine[A, B] {
    def name: String = s"${first.name} orElse ${second.name}"
    protected[refine] def runAt(a: A, at: Path): Verdict[B] = first.runAt(a, at) match {
      case Verdict.Declined(d) => second.runAt(a, at) match {
        case Verdict.Took(b, by, d2) => Verdict.Took(b, by, d ++ d2)
        case Verdict.Unclear(cs, d2) => Verdict.Unclear(cs, d ++ d2)
        case Verdict.Declined(d2) => Verdict.Declined(d ++ d2)
      }
      case taken => taken
    }
    protected[refine] def writeBack(b: B): Either[String, A] = first.writeBack(b) match {
      case Left(_) => second.writeBack(b)
      case ok => ok
    }
  }

  final case class Both[A, B, C, D](left: Refine[A, B], right: Refine[C, D]) extends Refine[(A, C), (B, D)] {
    def name: String = s"${left.name}*${right.name}"
    protected[refine] def runAt(a: (A, C), at: Path): Verdict[(B, D)] = product(left.runAt(a._1, at), right.runAt(a._2, at), at)
    protected[refine] def writeBack(b: (B, D)): Either[String, (A, C)] =
      for { x <- left.writeBack(b._1); y <- right.writeBack(b._2) } yield (x, y)
  }

  final case class Sum[A, B, C, D](left: Refine[A, B], right: Refine[C, D]) extends Refine[Either[A, C], Either[B, D]] {
    def name: String = s"${left.name}+${right.name}"
    protected[refine] def runAt(a: Either[A, C], at: Path): Verdict[Either[B, D]] = a match {
      case Left(x) => left.runAt(x, at) match {
        case Verdict.Took(b, by, d) => Verdict.Took(Left(b), by, d)
        case Verdict.Unclear(cs, d) => Verdict.Unclear(cs.map { case (by, b) => (by, Left(b)) }, d)
        case Verdict.Declined(d) => Verdict.Declined(d)
      }
      case Right(y) => right.runAt(y, at) match {
        case Verdict.Took(b, by, d) => Verdict.Took(Right(b), by, d)
        case Verdict.Unclear(cs, d) => Verdict.Unclear(cs.map { case (by, b) => (by, Right(b)) }, d)
        case Verdict.Declined(d) => Verdict.Declined(d)
      }
    }
    protected[refine] def writeBack(b: Either[B, D]): Either[String, Either[A, C]] = b match {
      case Left(x) => left.writeBack(x).map(Left(_))
      case Right(y) => right.writeBack(y).map(Right(_))
    }
  }

  final case class And[A, B, C](left: Refine[A, B], right: Refine[A, C], merge: Merge[A]) extends Refine[A, (B, C)] {
    def name: String = s"${left.name}&${right.name}"
    protected[refine] def runAt(a: A, at: Path): Verdict[(B, C)] = product(left.runAt(a, at), right.runAt(a, at), at)
    protected[refine] def writeBack(b: (B, C)): Either[String, A] =
      for { x <- left.writeBack(b._1); y <- right.writeBack(b._2); xy <- merge.merge(x, y) } yield xy
  }
}
