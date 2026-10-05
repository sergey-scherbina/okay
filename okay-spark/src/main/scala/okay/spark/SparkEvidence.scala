package okay.spark

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import org.apache.spark.TaskContext
import org.apache.spark.rdd.RDD

/** Summaries returned by successful Spark tasks; publish only after sink acknowledgement. */
object SparkEvidence:
  trait Canonical[-A] extends Serializable:
    def bytes(value: A): Array[Byte]

  final case class Partition(id: Int, attempt: Long, count: Long, digest: String) extends Serializable
  final case class Manifest(runId: String, partitions: Vector[Partition], digest: String)
  final case class Receipt(output: String, acknowledgement: String)
  final case class Committed(manifest: Manifest, receipt: Receipt)

  private def hex(bytes: Array[Byte]): String = java.util.HexFormat.of().formatHex(bytes)
  private def framed(md: MessageDigest, bytes: Array[Byte]): Unit =
    md.update(ByteBuffer.allocate(4).putInt(bytes.length).array())
    md.update(bytes)

  /** The sink must durably commit its output and manifest before returning.
    * Exceptions propagate: a failed sink produces no Committed value. */
  def run[A](rdd: RDD[A], runId: String)(canonical: Canonical[A])(sink: Manifest => Receipt): Committed =
    require(runId.trim.nonEmpty, "runId is empty")
    val summaries = rdd.mapPartitionsWithIndex { (id, values) =>
      val md = MessageDigest.getInstance("SHA-256")
      var count = 0L
      values.foreach { value =>
        framed(md, canonical.bytes(value))
        count += 1
      }
      Iterator.single(Partition(id, TaskContext.get().taskAttemptId(), count, hex(md.digest())))
    }.collect().toVector.sortBy(_.id)
    val md = MessageDigest.getInstance("SHA-256")
    framed(md, runId.getBytes(UTF_8))
    summaries.foreach { p => framed(md, s"${p.id}:${p.count}:${p.digest}".getBytes(UTF_8)) }
    val manifest = Manifest(runId, summaries, hex(md.digest()))
    val receipt = sink(manifest)
    require(receipt.output.trim.nonEmpty && receipt.acknowledgement.trim.nonEmpty, "sink returned an empty receipt")
    Committed(manifest, receipt)
