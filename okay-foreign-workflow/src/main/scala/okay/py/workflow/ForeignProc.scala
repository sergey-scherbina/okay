package okay.foreign.workflow

import okay.Wf
import okay.codec.Schema
import okay.foreign.{Condition, PyValue, Shape, ToPy}

/**
 * Foreign calls as leaves of a static `Proc` (foreign-workflow stage 2):
 * the same activity as `ForeignActivity.call`, in the arrow form whose
 * leaves are known before it runs. A leaf is NAMED by the function's
 * address, so `leaves` lists the far functions a procedure may call and
 * `mermaid` draws them; `Wf.Proc.program` runs it through the SAME oracle
 * and journal, so a term and a do-notation workflow asking the same calls
 * share one topic record for record.
 *
 * {{{
 * ForeignProc.call[String, Double]("shop:price") >>> ...     // a term
 * Proc.direct[ForeignProc.Sig, String, String]: sku =>        // proc-notation
 *   ForeignProc.decode[Double](!price(sku))
 * }}}
 */
object ForeignProc:

  /** the signature a foreign procedure is written over */
  type Sig = Wf.Asked[ForeignCall, String]

  private def A = okay.Proc.procArrow[Sig]

  /** a leaf calling `address` with one argument, its answer decoded */
  def call[X: ToPy, Out: Schema](address: String)(using Shape): Wf.Proc[ForeignCall, String, X, Either[Condition, Out]] =
    A.compose(A.arr(decode[Out]),
      Wf.Proc.asking[ForeignCall, String, X](address)(x => ForeignCall(address, Vector(ToPy(x)))))

  /** the same with two arguments, taken as a pair */
  def call2[X1: ToPy, X2: ToPy, Out: Schema](address: String)
                                            (using Shape): Wf.Proc[ForeignCall, String, (X1, X2), Either[Condition, Out]] =
    A.compose(A.arr(decode[Out]),
      Wf.Proc.asking[ForeignCall, String, (X1, X2)](address)(x => ForeignCall(address, Vector(ToPy(x._1), ToPy(x._2)))))

  /** for proc-notation: the QUESTION a helper hands to `!`, the helper's
   * name being what the picture shows */
  def ask(address: String)(args: PyValue*): Wf.Question[ForeignCall, String, String] =
    Wf.Question.Ask(ForeignCall(address, args.toVector))

  /** a journalled answer, read as `Out` */
  def decode[Out: Schema](written: String)(using shape: Shape): Either[Condition, Out] =
    ForeignActivity.answer(written).flatMap(shape.decode[Out])
