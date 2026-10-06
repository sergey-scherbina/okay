package okay.cont

/** A DIALOGUE: a body that asks questions; each `ask` captures the rest as a value the driver answers when it
 * will — a paused program, resumable once or many times, journaled or replayed by whoever holds it */
enum Question[Q, A, +X]:
  case Asked[Q, A](q: Q) extends Question[Q, A, A]
/** the dialogue paused at a question, its rest a program at the level `D` given the answer; or done */
enum Paused[Q, A, R, D <: Tuple]:
  case Ask[Q, A, R, D <: Tuple](q: Q, resume: A => Cont[D, D, Paused[Q, A, R, D]]) extends Paused[Q, A, R, D]
  case Done[Q, A, R, D <: Tuple](r: R) extends Paused[Q, A, R, D]
/** a question as a clause at the level `D`: the question and the rest given its answer, a program at `D` */
final class Pausing[Q, A, R, D <: Tuple] extends Clause[[X] =>> Question[Q, A, X], D, Paused[Q, A, R, D]]:
  def apply[X](op: Question[Q, A, X], k: X => Cont[D, D, Paused[Q, A, R, D]]): Cont[D, D, Paused[Q, A, R, D]] = op match
    case Question.Asked(q) => pure(Paused.Ask(q, a => k(a)))
/** `dialogue(body)`: the body paused at its first question, or done */
def dialogue[Q, A, R](using o: Ctx)
          (body: Handling[[X] =>> Question[Q, A, X], Paused[Q, A, R, o.Here], o.type] ?=> Cont[At[o.Here, Paused[Q, A, R, o.Here]] *: o.Here, At[o.Here, Paused[Q, A, R, o.Here]] *: o.Here, R])
  : Cont[o.Here, o.Here, Paused[Q, A, R, o.Here]] =
  handle[[X] =>> Question[Q, A, X], R, Paused[Q, A, R, o.Here]](Paused.Done(_))(Pausing[Q, A, R, o.Here]())(body)
/** a question; the answer, when the driver gives it */
def question[Q, A](q: Q)(using c: In[?, ?], p: Perform[[X] =>> Question[Q, A, X], c.type]): c.Body[A] =
  perform[[X] =>> Question[Q, A, X], A](Question.Asked(q))
object Paused:
  /** a dialogue at the top driven to its end by `answer` */
  @scala.annotation.tailrec
  def drive[Q, A, R](p: Paused[Q, A, R, EmptyTuple])(answer: Q => A): R = p match
    case Paused.Done(r) => r
    case Paused.Ask(q, resume) => drive(Machine.value(resume(answer(q))))(answer)
