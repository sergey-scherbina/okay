package okay.cont

/** READER, answering in place: the environment `R` is the handler's, read where `ask` is performed */
enum Reader[R, +A]:
  case Ask[R]() extends Reader[R, R]
  case Asks[R, A](f: R => A) extends Reader[R, A]
final class ReaderOf[R, A](r: R) extends Answering[[X] =>> Reader[R, X], A, A]:
  def ret(a: A): A = a
  def value[X](op: Reader[R, X]): X = op match
    case Reader.Ask() => r
    case Reader.Asks(f) => f(r)
/** `reader(r)(body)`: the body with `ask` answered `r` */
def reader[R, A](r: R)(using o: Ctx)
          (body: Answers[[X] =>> Reader[R, X], A, o.type] ?=> Cont[At[o.Here, A] *: o.Here, At[o.Here, A] *: o.Here, A])
  : Cont[o.Here, o.Here, A] =
  handle[[X] =>> Reader[R, X], A, A](ReaderOf[R, A](r))(body)
/** the environment */
def ask[R](using c: In[?, ?], p: Perform[[X] =>> Reader[R, X], c.type]): c.Body[R] =
  perform[[X] =>> Reader[R, X], R](Reader.Ask[R]())
/** a view of the environment */
def asks[R, A](f: R => A)(using c: In[?, ?], p: Perform[[X] =>> Reader[R, X], c.type]): c.Body[A] =
  perform[[X] =>> Reader[R, X], A](Reader.Asks(f))
