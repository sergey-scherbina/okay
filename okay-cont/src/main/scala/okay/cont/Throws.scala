package okay.cont

/** THROWS, an abort: `raise` is an operation whose clause never resumes — the handler's delimiter answers
 * `Left(e)` in place of the body, whatever was between (a capture through delimiters crossed, dropped) */
enum Throws[E, +A]:
  case Raise[E](e: E) extends Throws[E, Nothing]
final class Either_[E, A] extends Handler[[X] =>> Throws[E, X], A, Either[E, A]]:
  def ret(a: A): Either[E, A] = Right(a)
  def apply[X, Oc <: Ctx](using o: Oc)(op: Throws[E, X], k: X => Cont[o.Here, o.Here, Either[E, A]]): Cont[o.Here, o.Here, Either[E, A]] =
    op match
      case Throws.Raise(e) => pure(Left(e))
/** `throws(body)`: the body's value as `Right`, or the first `raise` as `Left` */
def throws[E, A](using o: Ctx)
          (body: Handling[[X] =>> Throws[E, X], Either[E, A], o.type] ?=> Cont[At[o.Here, Either[E, A]] *: o.Here, At[o.Here, Either[E, A]] *: o.Here, A])
  : Cont[o.Here, o.Here, Either[E, A]] =
  handle[[X] =>> Throws[E, X], A, Either[E, A]](Either_[E, A]())(body)
/** the failure: a program of any value, which no continuation ever gets */
def raise[E, A](e: E)(using c: In[?, ?], p: Perform[[X] =>> Throws[E, X], c.type]): c.Body[A] =
  perform[[X] =>> Throws[E, X], Nothing](Throws.Raise(e))
