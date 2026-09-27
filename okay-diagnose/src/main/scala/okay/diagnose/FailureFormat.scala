package okay.diagnose

/**
 * HOW A DIAGNOSIS JOINS A FAILURE, as a typeclass (specs/okay-diagnose.md;
 * the operator's rule, 2026-09-27: every dependency behind an abstraction
 * of ours, optional). okay-diagnose knows no framework and no library. Ours, the
 * default, keeps the failure's class and cause and adds the diagnosis as
 * a SUPPRESSED exception, which every framework prints. A framework whose
 * assertions carry a message the runner renders, munit's `withMessage` for
 * instance, has its own instance behind an import (okay-test's `Munit.failures`).
 */
trait FailureFormat:
  def extend(e: Throwable, diagnosis: String): Throwable

object FailureFormat:
  /** the diagnosis on a throwable, where it has none of its own */
  final class Diagnosis(msg: String) extends RuntimeException(msg)

  given suppressed: FailureFormat with
    def extend(e: Throwable, diagnosis: String): Throwable =
      if diagnosis.nonEmpty then e.addSuppressed(Diagnosis(diagnosis))
      e
