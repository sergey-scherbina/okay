package okay.r

import okay.py.{ForeignEval, PyStream}

/**
 * An R function over a vector as an okay STAGE over chunks
 * (foreign-streaming) — the one foreign stage (`okay.py.PyStream`) at R's
 * value rules. The function receives up to `chunk` elements as one vector
 * and answers a vector of any length; the next pull waits for it.
 * STATEFUL: a held CLOSURE (`R.hold` of a function that returns a function)
 * is called per chunk through `base::do.call`, and a second held closure
 * may flush at the end (`RRef.stage`).
 */
object RStream:

  type Row[I, O] = PyStream.Row[I, O]

  /** a call answered a condition: the stage cannot tell a `Left`, so it ends */
  type Failed = PyStream.Failed

  /** call a held closure with one argument, the chunk */
  private[r] def viaClosure(f: RRef, args: Vector[RValue]): ForeignEval[Either[Condition, RValue]] =
    ForeignEval.Call("base::do.call", Vector(RValue.Ref(f), RValue.Vec(args)))
