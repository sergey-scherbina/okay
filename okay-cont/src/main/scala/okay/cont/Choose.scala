package okay.cont

/** CHOICE, multi-shot: `among(as)` is answered once per element, the continuation resumed for each, and the
 * handler's delimiter answers every value the body came to, in order */
enum Choose[+A]:
  case Among[A](as: Seq[A]) extends Choose[A]
final class Every[A] extends Handler[Choose, A, Seq[A]]:
  def ret(a: A): Seq[A] = Seq(a)
  def apply[X, Oc <: Ctx](using o: Oc)(op: Choose[X], k: X => Cont[o.Here, o.Here, Seq[A]]): Cont[o.Here, o.Here, Seq[A]] =
    op match
      case Choose.Among(as) =>
        as.foldLeft(pure[Seq[A], o.Here](Seq.empty[A]))((acc, x) => acc.flatMap(s => k(x).map(s ++ _)))
/** `choose(body)`: every value of the body, one per path through its choices */
def choose[A](using o: Ctx)
          (body: Handling[Choose, Seq[A], o.type] ?=> Cont[At[o.Here, Seq[A]] *: o.Here, At[o.Here, Seq[A]] *: o.Here, A])
  : Cont[o.Here, o.Here, Seq[A]] =
  handle[Choose, A, Seq[A]](Every[A]())(body)
/** one of `as`, each in its own path */
def among[A](as: Seq[A])(using c: In[?, ?], p: Perform[Choose, c.type]): c.Body[A] =
  perform[Choose, A](Choose.Among(as))
