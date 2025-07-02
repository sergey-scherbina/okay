package okay

/*
inline def state[A, S, R](f: S => (A, S)): A ! S / R =
  shift(k => s => Function.uncurried(k).tupled(f(s)))
inline def runState[S, R](e: R ! S / R): S / R = e / (r => s => r)
inline def update[S, R](f: S => S): S ! S / R = state(s => (s, f(s)))
inline def get[S, R]: S ! S / R = update(identity)
inline def set[S, R](s: S): S ! S / R = update(_ => s)
inline def increase[R]: Int ! Int / R = update(_ + 1)
*/
