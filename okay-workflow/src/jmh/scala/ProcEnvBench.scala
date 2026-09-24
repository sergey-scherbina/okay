package okay.bench

import okay.*
import okay.Direct.*
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, Warmup}
import org.openjdk.jmh.annotations.State as JmhState
import scala.language.implicitConversions

/**
 * THE NOTATION'S ENVIRONMENT, PRICED (proc-notation-liveness). A block of N
 * statements where each name is read only by the NEXT one: every name but
 * the last is dead as soon as it has been read. Without liveness the
 * environment is a left-nested tuple that grows to depth N, every leaf
 * rebuilds it and every projection walks it; with liveness it stays at
 * depth two. The procedure is run by `foldMap` into the identity monad with
 * every question answered at once, so what is measured is the plumbing and
 * nothing else.
 */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(java.util.concurrent.TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class ProcEnvBench:
  type Sig = Wf.Asked[String, String]
  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)

  /** the identity monad, here only: the plumbing is all a run does */
  given idMonad: Monad[Id] with
    def pure[A](a: A): A = a
    extension [A](a: A) def flatMap[B](f: A => B): B = f(a)

  /** every question answered at once with its own text plus one character */
  val answer: [Z] => Wf.Question[String, String, Z] => Id[Z] = [Z] => (q: Wf.Question[String, String, Z]) =>
    q match
      case Wf.Question.Ask(s) => s + "x"
      case other => throw IllegalStateException(s"only asks here, got $other")

  val chain4: Wf.Proc[String, String, Unit, Int] =
    Proc.direct[Sig, Unit, Int]: _ =>
      val a0 = !ask("0")
      val a1 = !ask(a0)
      val a2 = !ask(a1)
      val a3 = !ask(a2)
      a3.length

  val chain16: Wf.Proc[String, String, Unit, Int] =
    Proc.direct[Sig, Unit, Int]: _ =>
      val a0 = !ask("0")
      val a1 = !ask(a0)
      val a2 = !ask(a1)
      val a3 = !ask(a2)
      val a4 = !ask(a3)
      val a5 = !ask(a4)
      val a6 = !ask(a5)
      val a7 = !ask(a6)
      val a8 = !ask(a7)
      val a9 = !ask(a8)
      val a10 = !ask(a9)
      val a11 = !ask(a10)
      val a12 = !ask(a11)
      val a13 = !ask(a12)
      val a14 = !ask(a13)
      val a15 = !ask(a14)
      a15.length

  val chain64: Wf.Proc[String, String, Unit, Int] =
    Proc.direct[Sig, Unit, Int]: _ =>
      val a0 = !ask("0")
      val a1 = !ask(a0)
      val a2 = !ask(a1)
      val a3 = !ask(a2)
      val a4 = !ask(a3)
      val a5 = !ask(a4)
      val a6 = !ask(a5)
      val a7 = !ask(a6)
      val a8 = !ask(a7)
      val a9 = !ask(a8)
      val a10 = !ask(a9)
      val a11 = !ask(a10)
      val a12 = !ask(a11)
      val a13 = !ask(a12)
      val a14 = !ask(a13)
      val a15 = !ask(a14)
      val a16 = !ask(a15)
      val a17 = !ask(a16)
      val a18 = !ask(a17)
      val a19 = !ask(a18)
      val a20 = !ask(a19)
      val a21 = !ask(a20)
      val a22 = !ask(a21)
      val a23 = !ask(a22)
      val a24 = !ask(a23)
      val a25 = !ask(a24)
      val a26 = !ask(a25)
      val a27 = !ask(a26)
      val a28 = !ask(a27)
      val a29 = !ask(a28)
      val a30 = !ask(a29)
      val a31 = !ask(a30)
      val a32 = !ask(a31)
      val a33 = !ask(a32)
      val a34 = !ask(a33)
      val a35 = !ask(a34)
      val a36 = !ask(a35)
      val a37 = !ask(a36)
      val a38 = !ask(a37)
      val a39 = !ask(a38)
      val a40 = !ask(a39)
      val a41 = !ask(a40)
      val a42 = !ask(a41)
      val a43 = !ask(a42)
      val a44 = !ask(a43)
      val a45 = !ask(a44)
      val a46 = !ask(a45)
      val a47 = !ask(a46)
      val a48 = !ask(a47)
      val a49 = !ask(a48)
      val a50 = !ask(a49)
      val a51 = !ask(a50)
      val a52 = !ask(a51)
      val a53 = !ask(a52)
      val a54 = !ask(a53)
      val a55 = !ask(a54)
      val a56 = !ask(a55)
      val a57 = !ask(a56)
      val a58 = !ask(a57)
      val a59 = !ask(a58)
      val a60 = !ask(a59)
      val a61 = !ask(a60)
      val a62 = !ask(a61)
      val a63 = !ask(a62)
      a63.length

  private val run4 = chain4.foldMap[Id](answer)
  private val run16 = chain16.foldMap[Id](answer)
  private val run64 = chain64.foldMap[Id](answer)

  @Benchmark def chain_4: Int = run4(())
  @Benchmark def chain_16: Int = run16(())
  @Benchmark def chain_64: Int = run64(())
