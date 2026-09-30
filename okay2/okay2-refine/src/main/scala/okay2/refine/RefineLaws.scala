package okay2.refine

import scala.util.control.NonFatal

/**
 * THE LAWS OF A PATTERN, checked on any pattern and any samples — okay's
 * `RefineLaws` (specs/refine.md, refine-laws) on the Scala 2 core:
 * read→write→read to the same value, write→read, nothing throws, the
 * same input reads the same verdict twice; `Expect.Corpus` also reports
 * every input not taken. Framework-free: a test asserts `report.ok`.
 */
object RefineLaws {

  sealed trait Expect
  object Expect {
    case object Laws extends Expect
    case object Corpus extends Expect
  }

  sealed trait Finding
  object Finding {
    final case class ReadBackDiffers(sample: String, read: String, reread: String) extends Finding
    final case class WriteRefused(sample: String, value: String, why: String) extends Finding
    final case class Threw(sample: String, where: String, error: String) extends Finding
    final case class NotDeterministic(sample: String, first: String, second: String) extends Finding
    final case class NotTaken(sample: String, verdict: String) extends Finding
  }

  final case class Report(inputs: Int, took: Int, values: Int, findings: Vector[Finding]) {
    def ok: Boolean = findings.isEmpty
    override def toString: String = {
      val head = s"RefineLaws: $inputs input(s), $took taken, $values value(s) — " +
        (if (ok) "every law holds" else s"${findings.length} finding(s)")
      (head +: findings.take(20).map(f => "  " + f.toString)).mkString("\n") +
        (if (findings.length > 20) s"\n  … ${findings.length - 20} more" else "")
    }
  }

  private def short(x: Any): String = {
    val s = x match {
      case bs: Array[Byte] => s"${bs.length} bytes"
      case other => String.valueOf(other)
    }
    if (s.length > 160) s.take(157) + "..." else s
  }

  private def attempt[T](f: => T): Either[Throwable, T] =
    try Right(f) catch { case NonFatal(e) => Left(e) }

  private def describe(e: Throwable): String = s"${e.getClass.getSimpleName}: ${e.getMessage}"

  def checkNamed[A, B](r: Refine[A, B], inputs: Iterable[(String, A)], values: Iterable[B] = Nil,
                       expect: Expect = Expect.Laws): Report = {
    val found = Vector.newBuilder[Finding]
    var took = 0
    for ((name, a) <- inputs) {
      attempt(r.run(a)) match {
        case Left(e) => found += Finding.Threw(name, "read", describe(e))
        case Right(v) =>
          attempt(r.run(a)) match {
            case Right(v2) if v2 != v => found += Finding.NotDeterministic(name, short(v), short(v2))
            case _ => ()
          }
          v match {
            case Verdict.Took(b, _, _) =>
              took += 1
              roundTrip(r, name, b).foreach(found += _)
            case other =>
              if (expect == Expect.Corpus) found += Finding.NotTaken(name, notTaken(other))
          }
      }
    }
    var n = 0
    for (b <- values) {
      n += 1
      roundTrip(r, s"value #$n", b).foreach(found += _)
    }
    Report(inputs.size, took, n, found.result())
  }

  def check[A, B](r: Refine[A, B], inputs: Iterable[A], values: Iterable[B] = Nil, expect: Expect = Expect.Laws): Report =
    checkNamed(r, inputs.zipWithIndex.map { case (a, i) => (s"input #${i + 1}", a) }, values, expect)

  private def roundTrip[A, B](r: Refine[A, B], name: String, b: B): Option[Finding] =
    attempt(r.write(b)) match {
      case Left(e) => Some(Finding.Threw(name, "write", describe(e)))
      case Right(Left(why)) => Some(Finding.WriteRefused(name, short(b), why))
      case Right(Right(a2)) =>
        attempt(r.run(a2)) match {
          case Left(e) => Some(Finding.Threw(name, "read of what was written", describe(e)))
          case Right(Verdict.Took(b2, _, _)) if b2 == b => None
          case Right(Verdict.Took(b2, _, _)) => Some(Finding.ReadBackDiffers(name, short(b), short(b2)))
          case Right(other) => Some(Finding.ReadBackDiffers(name, short(b), notTaken(other)))
        }
    }

  private def notTaken(v: Verdict[_]): String = v match {
    case Verdict.Unclear(cs, _) => s"unclear: ${cs.map(_._1).mkString(" | ")}"
    case Verdict.Declined(rs) => s"declined: ${rs.map(r => s"${r.at}: ${r.reason}").mkString("; ").take(300)}"
    case Verdict.Took(b, by, _) => s"took ${short(b)} by $by"
  }
}
