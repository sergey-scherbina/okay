package okay2.persist

import okay2.{Writer, pure}
import okay2.async.{Async, Timer}
import okay2.stream.{Chunk, ChunkBuf, Chunks, Source}

/**
 * Streaming reads over a topic (okay-persist's Streams.scala;
 * specs/persist.md, Interface): a `Source[Chunk[Record]]` — each chunk
 * one told value, each read one `Async` operation, constant memory for
 * any log size. `stream` ends when it catches up; `tail` never ends — at
 * `end` it parks on the platform timer and polls.
 *
 * Dropped history stops a stream by DECLARED decision: `Fail` (throw,
 * naming `begin`) or `Resume` (continue from `begin`, a stated jump).
 */
object Streams {

  sealed trait OnTooEarly
  object OnTooEarly {
    case object Fail extends OnTooEarly
    case object Resume extends OnTooEarly
  }

  final class DroppedHistory(val asked: Long, val begin: Long)
    extends RuntimeException(
      s"offset $asked is before the first retained record $begin — " +
        "history was dropped; resume from begin or from a snapshot")

  private type W = Chunk[Record]

  /** every record from `from` to the moment the stream catches up (a
   * read returning nothing ends it), `chunk` records per pull */
  def stream(t: Topic, partition: Int, from: Long, chunk: Int = 256,
             onTooEarly: OnTooEarly = OnTooEarly.Fail): Source[W] = {
    // each next read is deferred into the program's flatMap: no host stack
    def go(at: Long): Source[W] =
      Async(t.read(partition, at, chunk)).flatMap[Writer[W] with Async, Unit] {
        case Topic.Read.TooEarly(b) => tooEarly(at, b, onTooEarly)(go)
        case Topic.Read.Records(rs) =>
          if (rs.isEmpty) pure[Writer[W] with Async, Unit](())
          else Writer.tell[W](ChunkBuf.of(rs)).flatMap[Writer[W] with Async, Unit](_ => go(rs.last.offset + 1))
      }
    go(from)
  }

  /**
   * THE PARTITION AS A RECIPE (specs/dataflow.md, stage 11): a
   * `Chunks[Record]` that reads the partition from `from` in blocking
   * chunks and ends when a read returns nothing. BLOCKING on purpose —
   * a dataflow partition pulls until dry. `TooEarly` is a
   * `DroppedHistory` here, not a resume: a partition that silently
   * started later than asked would answer a different question.
   */
  def chunks(t: Topic, partition: Int, from: Long, chunk: Int = 256): Chunks[Record] = {
    // captured before the class: inside an `Iterator`, `partition` is
    // the method that splits one in two
    val part = partition
    val it = new Iterator[Record] {
      private var at = from
      private var buf: Iterator[Record] = Iterator.empty
      private var dry = false
      private def fill(): Unit =
        while (!buf.hasNext && !dry) {
          t.read(part, at, chunk) match {
            case Topic.Read.TooEarly(b) => throw new DroppedHistory(at, b)
            case Topic.Read.Records(rs) =>
              if (rs.isEmpty) dry = true
              else { at = rs.last.offset + 1; buf = rs.iterator }
          }
        }
      def hasNext: Boolean = { fill(); buf.hasNext }
      def next(): Record = { fill(); buf.next() }
    }
    Chunks.fromIterator(it, chunk)
  }

  /** the tailing read: like `stream`, but a caught-up reader parks
   * `pollMillis` on the platform timer and reads again — it never ends,
   * the consumer decides when to stop pulling */
  def tail(t: Topic, partition: Int, from: Long, chunk: Int = 256,
           pollMillis: Long = 25, onTooEarly: OnTooEarly = OnTooEarly.Fail)
          (implicit T: Timer): Source[W] = {
    def go(at: Long): Source[W] =
      Async(t.read(partition, at, chunk)).flatMap[Writer[W] with Async, Unit] {
        case Topic.Read.TooEarly(b) => tooEarly(at, b, onTooEarly)(go)
        case Topic.Read.Records(rs) =>
          if (rs.isEmpty) Async.sleep(pollMillis).flatMap[Writer[W] with Async, Unit](_ => go(at))
          else Writer.tell[W](ChunkBuf.of(rs)).flatMap[Writer[W] with Async, Unit](_ => go(rs.last.offset + 1))
      }
    go(from)
  }

  private def tooEarly(asked: Long, begin: Long, on: OnTooEarly)(resume: Long => Source[W]): Source[W] =
    on match {
      case OnTooEarly.Resume => resume(begin)
      case OnTooEarly.Fail => Async[Unit](throw new DroppedHistory(asked, begin))
    }
}
