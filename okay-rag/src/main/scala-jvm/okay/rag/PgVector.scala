package okay.rag

import okay.{!, +, Async, Chunk, Produce, Stream, async}
import okay.given
import okay.lex.Span
import okay.sql.{Sql, SqlValue}

/**
 * The vector store over the Sql seam (specs/data.md, the pgvector
 * box): a real database behind the SAME interface as the reference
 * MemoryStore — an adapter, exactly as Store.scala promised. Any
 * Sql driver serves it; the pg wire (okay-pg) is the natural one,
 * and this adapter is the pgvector consumer that road was cut for.
 *
 * OWN posture (specs/data.md): this is our table in our database —
 * `ensure()` creates the extension and the table, upsert is
 * `ON CONFLICT ... DO UPDATE` on the segment's identity (source +
 * span, the MemoryStore key), search is pushed to the ENGINE
 * (`ORDER BY embedding <op> query LIMIT k`), which is the entire
 * point of a vector database. Scores come back on the same scale
 * as `Vectors`: cosine similarity, plain dot, negated L2 — so the
 * memory engine and this one agree on a shared fixture.
 *
 * Exact scan v1 (no ivfflat/hnsw index): agreement with the
 * reference is testable only while search is exact; the
 * approximate index is a later, measured choice.
 */
final class PgVector(db: Sql, table: String, dim: Int,
                     metric: PgVector.Metric = PgVector.Metric.Cosine)
  extends VectorStore[Async]:
  import PgVector.*

  /** own-posture DDL: the extension, the table, the identity key */
  def ensure(): Unit ! Async =
    db.update("create extension if not exists vector").flatMap { _ =>
      db.update(
        s"""create table if not exists $table(
           source text not null,
           s_offset int not null, s_line int not null,
           s_column int not null, s_length int not null,
           content text not null,
           path text not null,
           embedding vector($dim) not null,
           primary key (source, s_offset, s_line, s_column, s_length))""").map(_ => ())
    }

  def upsert(items: Seq[(Segment, Embedding)]): Unit ! Async =
    val sql =
      s"""insert into $table values ($$1, $$2, $$3, $$4, $$5, $$6, $$7, $$8::vector)
         on conflict (source, s_offset, s_line, s_column, s_length)
         do update set content = excluded.content, path = excluded.path,
                       embedding = excluded.embedding"""
    val rows = items.map { (s, v) =>
      Vector[SqlValue](
        SqlValue.Text(s.source),
        SqlValue.I32(s.span.offset), SqlValue.I32(s.span.line),
        SqlValue.I32(s.span.column), SqlValue.I32(s.span.length),
        SqlValue.Text(s.text),
        SqlValue.Text(s.path.mkString("\u001f")),
        SqlValue.Text(vectorOf(v)))
    }
    db.batch(sql, okay.ChunkBuf.of(rows)).map(_ => ())

  def search(query: Embedding, k: Int): Seq[Scored] ! Async =
    val (op, score) = metric.sql
    val sql =
      s"""select source, s_offset, s_line, s_column, s_length, content, path,
                 $score as score
          from $table order by embedding $op $$1::vector limit $k"""
    drain(db.query(sql, Vector(SqlValue.Text(vectorOf(query))))).map { frames =>
      frames.map { f =>
        val seg = Segment(
          text(f(0)),
          Span(int(f(1)), int(f(2)), int(f(3)), int(f(4))),
          text(f(5)),
          text(f(6)).split('\u001f').toVector.filter(_.nonEmpty))
        Scored(seg, dbl(f(7)).toFloat)
      }
    }

  def delete(source: String, spans: Seq[Span]): Unit ! Async =
    val sql = s"""delete from $table where source = $$1 and s_offset = $$2
                  and s_line = $$3 and s_column = $$4 and s_length = $$5"""
    val rows = spans.map(sp => Vector[SqlValue](
      SqlValue.Text(source), SqlValue.I32(sp.offset), SqlValue.I32(sp.line),
      SqlValue.I32(sp.column), SqlValue.I32(sp.length)))
    if rows.isEmpty then okay.pure(())
    else db.batch(sql, okay.ChunkBuf.of(rows)).map(_ => ())

  def size: Int ! Async =
    drain(db.query(s"select count(*) from $table"))
      .map(fs => int(fs.head.head))

  /** everything out, for the doctor and the test */
  def truncate(): Unit ! Async = db.update(s"truncate $table").map(_ => ())

  private def drain(p: Chunk[Vector[SqlValue]] ! (Produce + Async))
  : Vector[Vector[SqlValue]] ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(Vector.empty)
      case Some((c, rest)) => drain(rest).map(c.toVector ++ _)
    }

object PgVector:

  /** the metric is declared, and the SCORE comes back on the same
   * scale as `Vectors`, so engines can agree on a fixture */
  enum Metric:
    case Cosine, Dot, L2

    /** (order-by operator, score expression over that operator) */
    private[rag] def sql: (String, String) = this match
      case Cosine => ("<=>", "1 - (embedding <=> $1::vector)")
      case Dot => ("<#>", "-(embedding <#> $1::vector)")
      case L2 => ("<->", "-(embedding <-> $1::vector)")

  /** pgvector's text literal */
  private[rag] def vectorOf(v: Embedding): String =
    val sb = new StringBuilder("[")
    var i = 0
    while i < v.length do
      if i > 0 then sb += ','
      sb.append(v(i))
      i += 1
    sb += ']'
    sb.result()

  private def text(v: SqlValue): String = v match
    case SqlValue.Text(s) => s
    case other => other.toString
  private def int(v: SqlValue): Int = v match
    case SqlValue.I32(x) => x
    case SqlValue.I64(x) => x.toInt
    case other => throw IllegalStateException(s"expected an int, got $other")
  private def dbl(v: SqlValue): Double = v match
    case SqlValue.F64(x) => x
    case SqlValue.I32(x) => x.toDouble
    case SqlValue.I64(x) => x.toDouble
    case SqlValue.Text(s) => s.toDouble
    case other => throw IllegalStateException(s"expected a number, got $other")
