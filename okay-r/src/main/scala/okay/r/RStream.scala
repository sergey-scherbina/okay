package okay.r

import okay.{!, %, +, Take, Writer, effect, pure}
import okay.codec.Schema

/**
 * An R function over a vector as an okay STAGE over chunks
 * (foreign-streaming) — okay-py's `PyStream` in R. The function receives
 * up to `chunk` elements as one vector and answers a vector of any length;
 * the next pull waits for it. STATEFUL: a held CLOSURE (`R.hold` of a
 * function that returns a function) is called per chunk through
 * `base::do.call`, and a second held closure may flush at the end.
 */
object RStream:

  type Row[I, O] = Take % I + (Writer % O + REval)

  final class Failed(val condition: Condition)
    extends RuntimeException(s"okay.r stage: ${condition.kind}: ${condition.message}")

  private[r] def chunked[I: ToR, O: Schema](
      chunk: Int,
      call: Vector[RValue] => REval[Either[Condition, RValue]],
      finish: Option[REval[Either[Condition, RValue]]]): Unit ! Row[I, O] =
    require(chunk >= 1, "okay.r stage: a chunk holds at least one element")
    type R = Row[I, O]

    def tellAll(answer: Either[Condition, RValue]): Unit ! R =
      answer.flatMap(RCodec.decode[Vector[O]](_)) match
        case Left(c) => throw Failed(c)
        case Right(os) => os.foldLeft(pure[R, Unit](()))((p, o) => p.flatMap(_ => effect[R, Unit](Writer(o))))

    def end: Unit ! R = finish match
      case None => pure(())
      case Some(op) => effect[R, Either[Condition, RValue]](op).flatMap(tellAll)

    def flush(buf: Vector[RValue], more: Boolean): Unit ! R =
      effect[R, Either[Condition, RValue]](call(buf)).flatMap(tellAll)
        .flatMap(_ => if more then fill(Vector.empty) else end)

    def fill(buf: Vector[RValue]): Unit ! R =
      if buf.size == chunk then flush(buf, more = true)
      else effect[R, Option[I]](Take.Await()).flatMap {
        case Some(i) => fill(buf :+ ToR(i))
        case None => if buf.isEmpty then end else flush(buf, more = false)
      }

    fill(Vector.empty)

  /** call a held closure with one argument, the chunk */
  private[r] def viaClosure(f: RRef, args: Vector[RValue]): REval[Either[Condition, RValue]] =
    REval.Call("base::do.call", Vector(RValue.Ref(f), RValue.Vec(args)))
