package okay

import ProbeVariance.{In, tell as inTell, get as inGet}

/** What the evidence buys over a plain free row parameter: the
 * discriminating case is a HELPER needing two effects, called from a
 * row that carries a third. */
object ProbeWhatInGives:

  type Mix3 = State % Int + Writer % String + Reader % Boolean

  // design A — a plain free row parameter; the effects are a PREFIX
  inline def tellA[W, F[+_]](w: W): Unit ! (Writer % W + F) = effect(Writer(w))
  inline def getA[S, F[+_]]: S ! (State % S + F) = effect(State.Get())

  def bumpA[F[+_]](by: Int): Int ! (State % Int + Writer % String + F) =
    for
      n <- getA[Int, Writer % String + F]
      _ <- tellA[String, State % Int + F](s"bump $n")
    yield n + by

  // design B — evidence; the row ITSELF is the return type
  def bumpB[R[+_]](by: Int)(using In[State % Int, R], In[Writer % String, R]): Int ! R =
    for
      n <- inGet[Int, R]
      _ <- inTell[String, R](s"bump $n")
    yield n + by

  val useA: Int ! Mix3 = bumpA[Reader % Boolean](1)
  val useB: Int ! Mix3 = bumpB[Mix3](1)

  def main(args: Array[String]): Unit =
    def run(p: Int ! Mix3): (Int, (Seq[String], Int)) =
      !.run(Reader.run[Boolean, (Int, (Seq[String], Int)), Pure](true)(
        State.handle[Int, (Seq[String], Int), Reader % Boolean](7)(
          Writer.run[String, Int, State % Int + Reader % Boolean](p))))
    println("A " + run(useA))
    println("B " + run(useB))
