package okay.cont

/** WRITER, answering in place: what is told goes into the handler's log, in order; the log and the value at the
 * end. A resumption shares the log, as `state` shares its cell: a body resumed twice tells twice into one log */
enum Writer[W, +A]:
  case Tell[W](w: W) extends Writer[W, Unit]
final class WriterLog[W, A] extends Answering[[X] =>> Writer[W, X], A, (List[W], A)]:
  private val log = List.newBuilder[W]
  def ret(a: A): (List[W], A) = (log.result(), a)
  def value[X](op: Writer[W, X]): X = op match
    case Writer.Tell(w) => log += w; ()
/** `writer(body)`: the body with `tell` logged; the log and the value */
def writer[W, A](using o: Ctx)
          (body: Answers[[X] =>> Writer[W, X], (List[W], A), o.type] ?=> Cont[At[o.Here, (List[W], A)] *: o.Here, At[o.Here, (List[W], A)] *: o.Here, A])
  : Cont[o.Here, o.Here, (List[W], A)] =
  handle[[X] =>> Writer[W, X], A, (List[W], A)](WriterLog[W, A]())(body)
/** a word to the log */
def tell[W](w: W)(using c: In[?, ?], p: Perform[[X] =>> Writer[W, X], c.type]): c.Body[Unit] =
  perform[[X] =>> Writer[W, X], Unit](Writer.Tell(w))
