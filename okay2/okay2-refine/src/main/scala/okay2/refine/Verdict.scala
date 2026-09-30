package okay2.refine

/**
 * What a run of a pattern says (okay's specs/refine.md §2, the Scala 2
 * port): the value with the path of names that produced it and every
 * alternative that declined; `Unclear` when more than one branch took
 * the input; `Declined` with every path tried and why. A decision a
 * reader cannot audit is not a decision a risk system may act on.
 */
sealed trait Verdict[+B] {
  /** the value, when exactly one branch took it */
  def toOption: Option[B] = this match {
    case Verdict.Took(b, _, _) => Some(b)
    case _ => None
  }

  /** every reason recorded on the way, takers' siblings included */
  def reasons: Vector[Refusal] = this match {
    case Verdict.Took(_, _, d) => d
    case Verdict.Unclear(_, d) => d
    case Verdict.Declined(d) => d
  }
}

object Verdict {
  final case class Took[+B](value: B, by: Path, declined: Vector[Refusal]) extends Verdict[B]
  final case class Unclear[+B](candidates: Vector[(Path, B)], declined: Vector[Refusal]) extends Verdict[B]
  final case class Declined(tried: Vector[Refusal]) extends Verdict[Nothing]
}

/** the names taken, root first; printed `text/json` */
final case class Path(steps: Vector[String]) {
  def /(name: String): Path = Path(steps :+ name)
  override def toString: String = steps.mkString("/")
}

object Path {
  val empty: Path = Path(Vector.empty)
  def apply(names: String*): Path = Path(names.toVector)
}

/** a name that declined, and its own words for why */
final case class Refusal(at: Path, reason: String) {
  override def toString: String = s"$at: $reason"
}
