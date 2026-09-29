package okay.refine

/**
 * What a run of a pattern says (specs/refine.md §2): the value with
 * the path of names that produced it and every alternative that
 * declined; `Unclear` when more than one branch took the input;
 * `Declined` with every path tried and why. specs/dlm.md's `Support`
 * makes the same call for utterances — a decision a reader cannot
 * audit is not a decision a risk system may act on.
 */
enum Verdict[+B]:
  case Took(value: B, by: Path, declined: Vector[Refusal])
  case Unclear(candidates: Vector[(Path, B)], declined: Vector[Refusal])
  case Declined(tried: Vector[Refusal])

  /** the value, when exactly one branch took it */
  def toOption: Option[B] = this match
    case Took(b, _, _) => Some(b)
    case _ => None

  /** every reason recorded on the way, takers' siblings included */
  def reasons: Vector[Refusal] = this match
    case Took(_, _, d) => d
    case Unclear(_, d) => d
    case Declined(d) => d

/** the names taken, root first; printed `text/json` */
final case class Path(steps: Vector[String]):
  def /(name: String): Path = Path(steps :+ name)
  override def toString: String = steps.mkString("/")

object Path:
  val empty: Path = Path(Vector.empty)
  def apply(names: String*): Path = Path(names.toVector)

/** a name that declined, and its own words for why */
final case class Refusal(at: Path, reason: String):
  override def toString: String = s"$at: $reason"
