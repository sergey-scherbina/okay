package okay.freer

import okay.*
import okay.given

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.freer.Shift.{push, reset, shift}

/**
 * CAPTURE DEPTH (delim-machine-allocs, 2026-09-27): what a capture
 * costs per frame between the `shift` and its prompt, per call of k.
 * DelimBenchmark's generator has about one such frame, so nothing
 * there prices the depth — and Logic's `choose`, which captures
 * through everything since its prompt, is the consumer it names.
 *
 * Two shapes of "a frame", because the machine sees them differently:
 *
 *   bind — `depth` plain `map`s around the shift. `Free.resume`
 *          rotates them into ONE composed continuation before the
 *          machine sees the operation, so `split` copies one frame and
 *          the depth is paid when k RUNS the composed function.
 *   push — `depth` delimiters of ANOTHER prompt around the shift, each
 *          with a `map` after it: every level is a mark and a bind on
 *          the machine's own stack, so `split` copies and `reify`
 *          re-walks every one, per capture and per call of k.
 *
 * k is called `shots` times from the shift's body, each answer summed,
 * so the slope in `shots` is the per-call price (reify + the rerun)
 * and the slope in `depth` at shots = 1 is the per-frame price.
 * Data only: no change depends on it (continuations-as-data-spike).
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DelimDepthBenchmark {

  @Param(Array("1", "16", "256"))
  var depth: Int = 0

  @Param(Array("1", "8"))
  var shots: Int = 0

  @Param(Array("bind", "push"))
  var shape: String = ""

  type Row = Shift % ? + Pure

  /** k called `shots` times in sequence, the answers summed */
  def callK(k: Unit => Int ! Row, n: Int, acc: Int): Int ! Row =
    if n == 0 then pure(acc) else k(()).flatMap(r => callK(k, n - 1, acc + r))

  def capture(p: Prompt[Int]): Int ! Row =
    shift[Int, Unit, Pure](p)(k => callK(k, shots, 0)).map(_ => 1)

  def binds(p: Prompt[Int], d: Int): Int ! Row =
    var prog = capture(p)
    var i = 0
    while i < d do
      prog = prog.map(_ + 1)
      i += 1
    prog

  def pushes(p: Prompt[Int], q: Prompt[Int], d: Int): Int ! Row =
    var prog = capture(p)
    var i = 0
    while i < d do
      prog = push[Int, Pure](q)(prog).map(_ + 1)
      i += 1
    prog

  @Benchmark
  def delimCaptureDepth(): Int =
    val q = Shift.prompt[Int]
    !.run(reset[Int, Pure] { p =>
      if shape == "bind" then binds(p, depth) else pushes(p, q, depth)
    })
}
