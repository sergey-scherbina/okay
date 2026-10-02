package okay.refine

import scala.util.control.NonFatal

/**
 * THE LAWS OF A PATTERN, checked on any pattern and any samples
 * (specs/refine.md, refine-laws) — so a domain repository's pattern
 * (okay-fin's FpML, okay-insure's QRTs) gets the checks this module's own
 * tests make, in one line. Framework-free: it answers a `Report`, and a
 * test asserts `report.ok` (munit, ScalaTest, a CLI over a corpus alike).
 *
 * What is checked:
 *  - READ, WRITE, READ: every input the pattern TAKES writes back, and
 *    what it writes reads back to the SAME value. Not the same bytes —
 *    a pattern writes the skeleton its read needs, and a path through
 *    `Format.value` writes JSON whatever it read — but the same value:
 *    that is what makes a path a conversion.
 *  - WRITE, READ: every sample VALUE writes, and reads back to itself.
 *  - NOTHING THROWS: a read declines in words, a write refuses with
 *    `Left`; an exception from either is a finding.
 *  - DETERMINISM: the same input reads to the same verdict twice.
 *
 * `Expect.Corpus` also reports every input that is not TAKEN — declined
 * (with the refusals) or `Unclear` (with the readings) — which is the
 * corpus method's "declined must be 0": the standard's own published
 * examples, every one read.
 */
object RefineLaws:

  /** how strict: the laws only, or the laws plus "every input is taken" */
  enum Expect:
    case Laws, Corpus

  /** one thing wrong, with the sample it was found on */
  enum Finding:
    case ReadBackDiffers(sample: String, read: String, reread: String)
    case WriteRefused(sample: String, value: String, why: String)
    case Threw(sample: String, where: String, error: String)
    case NotDeterministic(sample: String, first: String, second: String)
    case NotTaken(sample: String, verdict: String)

  /** what a check found: how many samples, how many were taken, and every finding */
  final case class Report(inputs: Int, took: Int, values: Int, findings: Vector[Finding]):
    def ok: Boolean = findings.isEmpty
    override def toString: String =
      val head = s"RefineLaws: $inputs input(s), $took taken, $values value(s) — " +
        (if ok then "every law holds" else s"${findings.length} finding(s)")
      (head +: findings.take(20).map(f => "  " + f.toString)).mkString("\n") +
        (if findings.length > 20 then s"\n  … ${findings.length - 20} more" else "")

  private def short(x: Any): String =
    val s = x match
      case bs: Array[Byte] => s"${bs.length} bytes"
      case other => String.valueOf(other)
    if s.length > 160 then s.take(157) + "..." else s

  private def attempt[T](f: => T): Either[Throwable, T] =
    try Right(f) catch case NonFatal(e) => Left(e)

  private def describe(e: Throwable): String = s"${e.getClass.getSimpleName}: ${e.getMessage}"

  /** the laws of `r` on named inputs (a file name, an example's id) and sample values */
  def checkNamed[A, B](r: Refine[A, B], inputs: Iterable[(String, A)], values: Iterable[B] = Nil,
                       expect: Expect = Expect.Laws): Report =
    val found = Vector.newBuilder[Finding]
    var took = 0
    for (name, a) <- inputs do
      attempt(r.run(a)) match
        case Left(e) => found += Finding.Threw(name, "read", describe(e))
        case Right(v) =>
          attempt(r.run(a)) match
            case Right(v2) if v2 != v => found += Finding.NotDeterministic(name, short(v), short(v2))
            case _ => ()
          v match
            case Verdict.Took(b, _, _) =>
              took += 1
              roundTrip(r, name, b).foreach(found += _)
            case other =>
              if expect == Expect.Corpus then found += Finding.NotTaken(name, notTaken(other))
    var n = 0
    for b <- values do
      n += 1
      roundTrip(r, s"value #$n", b).foreach(found += _)
    Report(inputs.size, took, n, found.result())

  /** the laws of `r` on inputs named by their position */
  def check[A, B](r: Refine[A, B], inputs: Iterable[A], values: Iterable[B] = Nil, expect: Expect = Expect.Laws): Report =
    checkNamed(r, inputs.zipWithIndex.map((a, i) => (s"input #${i + 1}", a)), values, expect)

  /** write `b`, read what was written: the same value, or a finding */
  private def roundTrip[A, B](r: Refine[A, B], name: String, b: B): Option[Finding] =
    attempt(r.write(b)) match
      case Left(e) => Some(Finding.Threw(name, "write", describe(e)))
      case Right(Left(why)) => Some(Finding.WriteRefused(name, short(b), why))
      case Right(Right(a2)) =>
        attempt(r.run(a2)) match
          case Left(e) => Some(Finding.Threw(name, "read of what was written", describe(e)))
          case Right(Verdict.Took(b2, _, _)) if b2 == b => None
          case Right(Verdict.Took(b2, _, _)) => Some(Finding.ReadBackDiffers(name, short(b), short(b2)))
          case Right(other) => Some(Finding.ReadBackDiffers(name, short(b), notTaken(other)))

  private def notTaken(v: Verdict[?]): String = v match
    case Verdict.Unclear(cs, _) => s"unclear: ${cs.map(_._1).mkString(" | ")}"
    case Verdict.Declined(rs) => s"declined: ${rs.map(r => s"${r.at}: ${r.reason}").mkString("; ").take(300)}"
    case Verdict.Took(b, by, _) => s"took ${short(b)} by $by"
