package okay.cont

/** EMIT: a body that yields. Two handlers: `collect` answers in place and keeps every element; `generate` is
 * LAZY — each `yield` captures the rest of the body as the next step, a program at the generator's level, run
 * when the consumer asks (at the top, `Machine.value`) */
enum Emit[W, +A]:
  case Yield[W](w: W) extends Emit[W, Unit]
final class Collecting[W, A] extends Answering[[X] =>> Emit[W, X], A, (List[W], A)]:
  private val out = List.newBuilder[W]
  def ret(a: A): (List[W], A) = (out.result(), a)
  def value[X](op: Emit[W, X]): X = op match
    case Emit.Yield(w) => out += w; ()
/** `collect(body)`: every element yielded, and the value */
def collect[W, A](using o: Ctx)
          (body: Answers[[X] =>> Emit[W, X], (List[W], A), o.type] ?=> Cont[At[o.Here, (List[W], A)] *: o.Here, At[o.Here, (List[W], A)] *: o.Here, A])
  : Cont[o.Here, o.Here, (List[W], A)] =
  handle[[X] =>> Emit[W, X], A, (List[W], A)](Collecting[W, A]())(body)
/** a lazy generator at the level `D`: the next element and the rest, a program there; or done */
enum Gen[W, D <: Tuple]:
  case Next[W, D <: Tuple](w: W, rest: Cont[D, D, Gen[W, D]]) extends Gen[W, D]
  case Done[W, D <: Tuple]() extends Gen[W, D]
/** `yield` as a clause at the level `D`: the element and the rest, `k(())`, a program at `D` */
final class Generating[W, D <: Tuple] extends Clause[[X] =>> Emit[W, X], D, Gen[W, D]]:
  def apply[X](op: Emit[W, X], k: X => Cont[D, D, Gen[W, D]]): Cont[D, D, Gen[W, D]] = op match
    case Emit.Yield(w) => pure(Gen.Next(w, k(())))
/** `generate(body)`: the body as a lazy generator at this level — nothing runs until the consumer pulls */
def generate[W](using o: Ctx)
          (body: Handling[[X] =>> Emit[W, X], Gen[W, o.Here], o.type] ?=> Cont[At[o.Here, Gen[W, o.Here]] *: o.Here, At[o.Here, Gen[W, o.Here]] *: o.Here, Unit])
  : Cont[o.Here, o.Here, Gen[W, o.Here]] =
  handle[[X] =>> Emit[W, X], Unit, Gen[W, o.Here]](_ => Gen.Done())(Generating[W, o.Here]())(body)
/** an element out */
def yield_[W](w: W)(using c: In[?, ?], p: Perform[[X] =>> Emit[W, X], c.type]): c.Body[Unit] =
  perform[[X] =>> Emit[W, X], Unit](Emit.Yield(w))
