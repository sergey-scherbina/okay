package bench.okay

// The okay rows' own control, in the shape of bench.direct.Validation:
// every okay lane computes what the Loom backend computes, on the same
// inputs, before any okay number is read. Copied into io-bench's test
// sources by apply.py; run as `ioBench/Test/runMain bench.okay.OkayValidation`.

import bench.direct.{ParallelBench, PrimitivesBench, RunnerBench}
import bench.matched.Work

object OkayValidation:
  private var checks = 0
  private def equal[A](actual: A, expected: A): Unit =
    assert(actual == expected, s"expected $expected, got $actual")
    checks += 1

  def main(args: Array[String]): Unit =
    for runtime <- Vector("okay", "okayOwn", "okayAdaptive") do
      val parallel = new ParallelBench
      parallel.runtime = runtime
      parallel.setupBackend()
      for size <- Vector(0, 1, 17, 4096); limit <- Vector(1, 8); rounds <- Vector(0, 64) do
        parallel.size = size
        parallel.parallelism = limit
        parallel.work = rounds
        parallel.setup()
        equal(parallel.workers(), Vector.tabulate(size)(Work(_, rounds)))
      val primitives = new PrimitivesBench
      primitives.runtime = runtime
      primitives.setupBackend()
      for ops <- Vector(0, 1, 17, 1000) do
        primitives.ops = ops
        primitives.setup()
        equal(primitives.spawnJoin(), ops.toLong * (ops - 1) / 2)
      val runner = new RunnerBench
      runner.runtime = runtime
      runner.setupBackend()
      equal(runner.entry(), 1)
    println(s"PASS $checks okay checks")
