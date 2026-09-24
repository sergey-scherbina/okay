package okay.pool

import okay.codec.Json
import java.nio.charset.StandardCharsets.UTF_8

/**
 * WHAT A RUN ID NAMES, BESIDE ITS EPOCH JOURNAL (specs/cluster-pool.md,
 * "the run id is the journal name"). `Checkpoint`'s own record
 * (`okay.cluster.Folded`) is keyed by the SINK's Schema and says
 * nothing about which job or parameters produced it — a `GET` from a
 * member that never saw the original `POST` needs that said somewhere
 * durable too, so `Pool` writes ONE of these, once, to a checkpoint of
 * its own naming (`"$id.meta"`) before it ever calls `Job.lead`.
 *
 * Plain bytes, not a `Schema`: `Checkpoint.save`/`latest` already ask
 * for nothing else, and `params` is a `Json` value with no `Schema`
 * of its own to derive against (it is validated against the JOB's
 * schema at submission, and re-validated the same way on every
 * resume — there is no second schema to agree with).
 */
private[pool] final case class RunMeta(job: String, params: Json, parts: Int, take: Int)

private[pool] object RunMeta:
  def encode(m: RunMeta): Array[Byte] =
    Json.print(Json.JObj(Vector(
      "job" -> Json.JStr(m.job),
      "params" -> m.params,
      "parts" -> Json.JNum(m.parts.toDouble),
      "take" -> Json.JNum(m.take.toDouble),
    ))).getBytes(UTF_8)

  def decode(bytes: Array[Byte]): Either[String, RunMeta] =
    Json.parse(new String(bytes, UTF_8)) match
      case Json.JObj(fs) =>
        val m = fs.toMap
        (m.get("job"), m.get("parts"), m.get("take")) match
          case (Some(Json.JStr(job)), Some(Json.JNum(parts)), Some(Json.JNum(take))) =>
            Right(RunMeta(job, m.getOrElse("params", Json.JObj(Vector.empty)), parts.toInt, take.toInt))
          case _ => Left("a run record must carry job, parts and take")
      case other => Left(s"a run record must be a JSON object, was ${Json.print(other).take(60)}")
