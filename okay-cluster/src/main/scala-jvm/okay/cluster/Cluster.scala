package okay.cluster

import okay.{Chunk, Chunks}

/**
 * Distribution of work (specs/cluster.md): chunks are the shipping
 * unit, the Aggregator triple is the merge contract, and the fault
 * model is per-chunk recompute — a pure Chunks source is a VALUE, so
 * a chunk already pulled is lineage in hand: when the worker holding
 * it dies, the chunk is simply given to a survivor.
 */
object Cluster {

  /** the work seam: one executor of chunk work — in-process, or a
   * wire away (send the chunk, await the partial); a dead worker
   * THROWS, that is the whole protocol */
  type Worker[A, Acc] = Chunk[A] => Acc

  /**
   * Drive a replayable chunked source over the workers, round-robin
   * over the living: a worker that throws is dead and leaves the
   * rotation, its chunk is recomputed on a survivor, the partials
   * merge by the aggregator's combOp (order-free by the P1 contract).
   * No workers left = the exception propagates: nothing to hide.
   */
  def distribute[A, Acc](source: Chunks[A], workers: Vector[Worker[A, Acc]])
                        (zero: Acc, merge: (Acc, Acc) => Acc): Acc =
    var alive = workers.indices.toVector
    var acc = zero
    var rest = source
    var turn = 0
    var pulled = Chunks.pull(rest)
    while pulled.isDefined do
      val (c, r) = pulled.get
      var done = false
      while !done do
        if alive.isEmpty then throw new IllegalStateException("no workers left")
        val w = alive(turn % alive.size)
        turn += 1
        try
          acc = merge(acc, workers(w)(c))
          done = true
        catch case _: Throwable =>
          alive = alive.filterNot(_ == w)   // dead; the chunk goes to a survivor
      rest = r
      pulled = Chunks.pull(rest)
    acc
}
