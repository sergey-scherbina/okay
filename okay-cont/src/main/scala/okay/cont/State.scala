package okay.cont

/** STATE, answering in place: the state in a cell of the handler, one per `handle`, so `get` and `put` are answered
 * where they are performed, no capture. A resumption shares the cell: a body resumed twice sees ONE state, the
 * second resumption the first's last — not a replay. The replay is the answer type's (TestState, `PState`) */
enum State[S, +A]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]
final class StateCell[S, A](var state: S) extends Answering[[X] =>> State[S, X], A, (S, A)]:
  def ret(a: A): (S, A) = (state, a)
  def value[X](op: State[S, X]): X = op match
    case State.Get() => state
    case State.Put(s) => state = s
/** `state(s0)(body)`: the body with `get`/`put` answered from a cell starting at `s0`; the last state and the value */
def state[S, A](s0: S)(using o: Ctx)
         (body: Answers[[X] =>> State[S, X], (S, A), o.type] ?=> Cont[At[o.Here, (S, A)] *: o.Here, At[o.Here, (S, A)] *: o.Here, A])
  : Cont[o.Here, o.Here, (S, A)] =
  handle[[X] =>> State[S, X], A, (S, A)](StateCell[S, A](s0))(body)

/** the state */
def get[S](using c: In[?, ?], p: Perform[[X] =>> State[S, X], c.type]): c.Body[S] =
  perform[[X] =>> State[S, X], S](State.Get[S]())
/** the state replaced */
def put[S](s: S)(using c: In[?, ?], p: Perform[[X] =>> State[S, X], c.type]): c.Body[Unit] =
  perform[[X] =>> State[S, X], Unit](State.Put(s))
/** the state changed by `f`; the new state */
def modify[S](f: S => S)(using c: In[?, ?], p: Perform[[X] =>> State[S, X], c.type]): c.Body[S] =
  get[S].flatMap(s => { val n = f(s); put(n).map(_ => n) })
