package okay.py

import okay.{!, %, +, Take, Writer, effect, pure}
import okay.codec.Schema

/**
 * A Python function, or a method of a held object, as an okay STAGE over
 * chunks (foreign-streaming, specs/foreign-highlevel.md stage 6):
 *
 * {{{
 * through(source.plus[ForeignEval])(Py.stage[Double, Double]("model:predict", chunk = 256))
 * }}}
 *
 * The stage pulls up to `chunk` elements, calls the function ONCE with
 * them as a list, and tells every element of the list it answers — any
 * length, so it filters or expands as well as maps. The next pull happens
 * after the answer: a slow model back-pressures its source, and one
 * message crosses per chunk, not per element. The input's end flushes a
 * partial chunk.
 *
 * STATEFUL transformations are a held object (foreign-object-handles):
 * `ref.stage[I, O]("step", finish = Some("flush"))` calls `step` per chunk
 * and `flush` once at the end, for what the object still holds.
 *
 * Pull-driven by design rather than a Python generator running beside
 * okay: a generator suspended on its input would need a second thread in
 * the shim, and a stage that owns the pull is what okay's `through`
 * already composes.
 */
object PyStream:

  /** the row of a Python stage: okay's stage row, plus the calls */
  type Row[I, O] = Take % I + (Writer % O + ForeignEval)

  /** a call answered a condition: the stage cannot tell a `Left`, so it
   * ends, naming what Python said */
  final class Failed(val condition: Condition)
    extends RuntimeException(s"okay.py stage: ${condition.kind}: ${condition.message}")

  private[py] def chunked[I: ToPy, O: Schema](
      chunk: Int,
      call: Vector[PyValue] => ForeignEval[Either[Condition, PyValue]],
      finish: Option[ForeignEval[Either[Condition, PyValue]]]): Unit ! Row[I, O] =
    require(chunk >= 1, "okay.py stage: a chunk holds at least one element")
    type R = Row[I, O]

    def tellAll(answer: Either[Condition, PyValue]): Unit ! R =
      answer.flatMap(PyCodec.decode[Vector[O]](_)) match
        case Left(c) => throw Failed(c)
        case Right(os) => os.foldLeft(pure[R, Unit](()))((p, o) => p.flatMap(_ => effect[R, Unit](Writer(o))))

    def end: Unit ! R = finish match
      case None => pure(())
      case Some(op) => effect[R, Either[Condition, PyValue]](op).flatMap(tellAll)

    def flush(buf: Vector[PyValue], more: Boolean): Unit ! R =
      effect[R, Either[Condition, PyValue]](call(buf)).flatMap(tellAll)
        .flatMap(_ => if more then fill(Vector.empty) else end)

    def fill(buf: Vector[PyValue]): Unit ! R =
      if buf.size == chunk then flush(buf, more = true)
      else effect[R, Option[I]](Take.Await()).flatMap {
        case Some(i) => fill(buf :+ ToPy(i))
        case None => if buf.isEmpty then end else flush(buf, more = false)
      }

    fill(Vector.empty)
