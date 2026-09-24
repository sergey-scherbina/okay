package okay.persist

import okay.{!, +, %, Async, Chunk, ChunkBuf, Chunks, Source, Timer, Writer, effect}

/**
 * Streaming reads over a topic (specs/persist.md, Interface): a
 * `Source[Chunk[Record]]` — each chunk is one told value, each read
 * one `Async` operation, constant memory for any log size (the
 * writer carrier since producer-to-writer-carrier, 2026-09-19; it was
 * `Chunk[Record] ! Produce + Async`, the `JdbcInterop` shape, whose
 * element sat in the answer position). `stream` ends when it catches
 * up; `tail` never ends — at `end` it parks on the platform timer
 * and polls, which is the contract the poll-on-end behavior test
 * guarantees engine-side (ui-durable, resumable SSE).
 *
 * Dropped history stops a stream by DECLARED decision, not
 * silently: a consumer whose offset aged out chooses `Fail` (throw,
 * naming `begin`) or `Resume` (continue from `begin`, a stated
 * jump). The decision-not-promise pattern once more.
 */
object Streams:

  enum OnTooEarly:
    case Fail, Resume

  final class DroppedHistory(val asked: Long, val begin: Long)
    extends RuntimeException(
      s"offset $asked is before the first retained record $begin — " +
        "history was dropped; resume from begin or from a snapshot")

  private type F = Writer % Chunk[Record] + Async

  /** every record from `from` to the moment the stream catches up
   * (a read returning nothing ends it), `chunk` records per pull */
  def stream(t: Topic, partition: Int, from: Long, chunk: Int = 256,
             onTooEarly: OnTooEarly = OnTooEarly.Fail)
  : Source[Chunk[Record]] =
    def go(at: Long): Source[Chunk[Record]] =
      effect[F, Topic.Read](Async.Run(() => t.read(partition, at, chunk))).flatMap {
        case Topic.Read.TooEarly(b) => tooEarly(at, b, onTooEarly)(go)
        case Topic.Read.Records(rs) =>
          if rs.isEmpty then okay.pure(())
          else effect[F, Unit](Writer(ChunkBuf.of(rs))).flatMap(_ => go(rs.last.offset + 1))
      }
    go(from)

  /**
   * THE PARTITION AS A RECIPE (specs/dataflow.md, stage 11): a
   * `Chunks[Record]` that reads the topic partition from `from` in
   * blocking chunks and ends when a read returns nothing.
   *
   * This is what the dataflow engine's `Flow.Src` holds a thunk to —
   * `Flow.of(Vector.tabulate(t.partitions)(p => () => chunks(t, p, from)))`
   * is a plan over a log — and it is three lines because both halves
   * already fit: okay-persist sees `Chunks`, and `Flow.of` takes any
   * thunk. No dependency runs in either direction.
   *
   * BLOCKING on purpose. `stream` above is the effectful producer for
   * a consumer that composes; a dataflow PARTITION runs on its own
   * fibre and pulls until the source is dry, and a plain iterator is
   * the whole of what it needs. A read that returns `TooEarly` — the
   * history before `from` was dropped — is a `DroppedHistory` here,
   * not a resume: a partition that silently started later than it
   * was asked to would answer a different question.
   */
  def chunks(t: Topic, partition: Int, from: Long, chunk: Int = 256): Chunks[Record] =
    // captured before the class: inside an `Iterator`, `partition`
    // is the method that splits one in two
    val part = partition
    val it = new Iterator[Record]:
      private var at = from
      private var buf: Iterator[Record] = Iterator.empty
      private var dry = false
      private def fill(): Unit =
        while !buf.hasNext && !dry do
          t.read(part, at, chunk) match
            case Topic.Read.TooEarly(b) => throw DroppedHistory(at, b)
            case Topic.Read.Records(rs) =>
              if rs.isEmpty then dry = true
              else { at = rs.last.offset + 1; buf = rs.iterator }
      def hasNext: Boolean = { fill(); buf.hasNext }
      def next(): Record = { fill(); buf.next() }
    Chunks.fromIterator(it, chunk)

  /** the tailing read: like `stream`, but a caught-up reader parks
   * `pollMillis` on the platform timer and reads again — it never
   * ends, the consumer decides when to stop pulling */
  def tail(t: Topic, partition: Int, from: Long, chunk: Int = 256,
           pollMillis: Long = 25, onTooEarly: OnTooEarly = OnTooEarly.Fail)
          (using Timer)
  : Source[Chunk[Record]] =
    def go(at: Long): Source[Chunk[Record]] =
      effect[F, Topic.Read](Async.Run(() => t.read(partition, at, chunk))).flatMap {
        case Topic.Read.TooEarly(b) => tooEarly(at, b, onTooEarly)(go)
        case Topic.Read.Records(rs) =>
          if rs.isEmpty then
            okay.!.widen[Unit, Async, Writer % Chunk[Record]](Async.sleep(pollMillis)).flatMap(_ => go(at))
          else effect[F, Unit](Writer(ChunkBuf.of(rs))).flatMap(_ => go(rs.last.offset + 1))
      }
    go(from)

  private def tooEarly(asked: Long, begin: Long, on: OnTooEarly)
                      (resume: Long => Source[Chunk[Record]]): Source[Chunk[Record]] =
    on match
      case OnTooEarly.Resume => resume(begin)
      case OnTooEarly.Fail =>
        effect[F, Unit](Async.Run(() => throw DroppedHistory(asked, begin)))
