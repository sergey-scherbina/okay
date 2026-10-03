package okay

import scala.util.Random

/**
 * THE REFERENCE AS AN ORACLE (specs/cont-js-depth.md): random programs
 * written against `Delimited` alone run on the frame machine and on
 * DelimitedReference — a list context, nothing subtle — and must agree,
 * a `NoPrompt` included. Hand-written laws pin a dozen shapes; the bugs
 * of an optimised machine hide in the combinations nobody writes by hand
 * (segment catenation, a resumption across a delimiter, `ret` under a
 * capture, multi-shot under nested prompts). On every platform; a
 * mismatch prints the seed and the program.
 */
object DelimitedDifferential:

  /** a program, as data, so a failure can be printed and replayed */
  enum Prog:
    case Pure(n: Int)
    case Bind(m: Prog, rest: Prog)                // m, then rest; answers mixed
    case Reset(p: Int, body: Prog)
    case Dollar(p: Int, retAdd: Int, body: Prog)  // ret = v => pure(v + retAdd)
    case Shift0(p: Int, body: Body)
    case Shift(p: Int, body: Body)
    case Abort(p: Int, n: Int)
    case RunHead(body: Prog)                      // the door: body run to its head form, mid-program

  /** what a capture's body does with its continuation */
  enum Body:
    case Once(a: Int)                    // k(a)
    case Twice(a: Int, b: Int)           // k(a), then k(b), answers mixed
    case Drop(n: Int)                    // never resumes: answers n
    case Then(a: Int, after: Prog)       // k(a), then a program outside, mixed
    case ResumeWith(m: Prog)             // resume k with a COMPUTATION
    case HeadAt(a: Int)                  // k(a) through the door's resumption form, runHeadAt

  /** two answers into one, order-sensitive, wrapping like Int does */
  def mix(x: Int, y: Int): Int = x * 31 + y

  /** the program on any instance; prompts fresh per run */
  def interpret[M[_, _, _]](D: LambdaDollar[M])(prog: Prog): M[Int, Int, Int] =
    given At = At("TestDelimitedDifferential")
    val ps = Vector.fill(3)(D.delimiter[Int, Int])
    def pure(n: Int): M[Int, Int, Int] = D.pure[Int, Int](n)
    def bind(m: M[Int, Int, Int])(f: Int => M[Int, Int, Int]): M[Int, Int, Int] = D.bind[Int, Int, Int, Int, Int](m)(f)
    def body(b: Body)(k: D.SubCont[Int, Int, Int, Int]): M[Int, Int, Int] = b match
      case Body.Once(a) => k(a)
      case Body.Twice(a, c) => bind(k(a))(x => bind(k(c))(y => pure(mix(x, y))))
      case Body.Drop(n) => pure(n)
      case Body.Then(a, after) => bind(k(a))(x => bind(go(after))(y => pure(mix(x, y))))
      case Body.ResumeWith(m) => D.resume(k)(go(m))
      case Body.HeadAt(a) => D.runHeadAt(k)(a)
    def go(p: Prog): M[Int, Int, Int] = p match
      case Prog.Pure(n) => pure(n)
      case Prog.Bind(m, rest) => bind(go(m))(x => bind(go(rest))(y => pure(mix(x, y))))
      case Prog.Reset(i, b) => D.reset[Int, Int, Int](ps(i))(go(b))
      case Prog.Dollar(i, add, b) => D.dollar[Int, Int, Int, Int](ps(i))(v => pure(v + add))(go(b))
      case Prog.Shift0(i, b) => D.shift0[Int, Int, Int, Int, Int](ps(i))(body(b))
      case Prog.Shift(i, b) => D.shift[Int, Int, Int, Int, Int](ps(i))(body(b))
      case Prog.Abort(i, n) => D.abort[Int, Int, Int](ps(i))(n)
      // the machine runs its loop here and hands back the head form — a capture to a delimiter OUTSIDE the
      // body leaves as an operation, and the outer run must place it (delimited-one-door); the reference's
      // runHead is the body itself, so the two agree only if the door does
      case Prog.RunHead(b) => D.runHead[Int, Int, Int](go(b))
    go(prog)

  /** an answer, or the failure's kind — the two must agree on both */
  def outcome[M[_, _, _]](D: LambdaDollar[M])(prog: Prog): Either[String, Int] =
    try Right(D.run(interpret(D)(prog)))
    catch case _: NoPrompt => Left("NoPrompt")

  /**
   * A random program of bounded size. Most captures sit under a delimiter
   * for their prompt (a top-level `Reset`/`Dollar` is likely), so most
   * programs answer; the rest test `NoPrompt` on both sides. `Twice` is
   * rarer and the depth small: multi-shot doubles the work, and the
   * reference recurses once per STEP of a run.
   */
  def gen(r: Random, depth: Int): Prog =
    def n = r.nextInt(7) - 3
    def p = r.nextInt(3)
    def body(d: Int): Body = r.nextInt(10) match
      case 0 | 1 | 2 => Body.Once(n)
      case 3 => Body.Twice(n, n)
      case 4 | 5 => Body.Drop(n)
      case 6 | 7 => Body.Then(n, gen(r, d - 1))
      case 8 => Body.ResumeWith(gen(r, d - 1))
      case _ => Body.HeadAt(n)
    if depth <= 0 then Prog.Pure(n)
    else r.nextInt(14) match
      case 0 => Prog.Pure(n)
      case 1 | 2 | 3 => Prog.Bind(gen(r, depth - 1), gen(r, depth - 1))
      case 4 | 5 => Prog.Reset(p, gen(r, depth - 1))
      case 6 => Prog.Dollar(p, n, gen(r, depth - 1))
      case 7 | 8 => Prog.Shift0(p, body(depth - 1))
      case 9 | 10 => Prog.Shift(p, body(depth - 1))
      case 11 => Prog.Abort(p, n)
      case _ => Prog.RunHead(gen(r, depth - 1))

  /** a program wrapped in delimiters for all three prompts, in a random order */
  def delimited(r: Random, depth: Int): Prog =
    r.shuffle(List(0, 1, 2)).foldLeft(gen(r, depth))((b, i) =>
      if r.nextBoolean() then Prog.Reset(i, b) else Prog.Dollar(i, r.nextInt(5), b))

class TestDelimitedDifferential extends munit.FunSuite:
  import DelimitedDifferential.*

  val machine = LambdaDollar.machine
  val reference = DelimitedReference.Ref

  /** `minAnswered`: the share of programs that must answer, so the
   * generator is shown to reach past `NoPrompt` — and at least one must
   * fail with it, so that side is compared too */
  def agree(label: String, seeds: Range, depth: Int, wrap: Boolean, minAnswered: Double): Unit =
    test(s"machine agrees with the reference: $label") {
      var answered = 0
      for seed <- seeds do
        val r = Random(seed)
        val prog = if wrap then delimited(r, depth) else gen(r, depth)
        val (m, ref) = (outcome(machine)(prog), outcome(reference)(prog))
        assertEquals(m, ref, s"seed $seed: $prog")
        if ref.isRight then answered += 1
      assert(answered >= seeds.size * minAnswered, s"only $answered of ${seeds.size} answered")
      assert(answered < seeds.size || !wrap, "no program failed with NoPrompt")
    }

  agree("2000 programs, every prompt delimited", 0 until 2000, depth = 5, wrap = true, minAnswered = 0.6)
  agree("2000 programs, unwrapped — NoPrompt agrees too", 2000 until 4000, depth = 4, wrap = false, minAnswered = 0.1)
  agree("500 deeper programs", 4000 until 4500, depth = 7, wrap = true, minAnswered = 0.6)

  test("the door is reached: generated programs run sub-programs through runHead, captures crossing it included") {
    val progs = (0 until 2000).map(seed => delimited(Random(seed), 5))
    def heads(p: Prog): Int = p match
      case Prog.RunHead(b) => 1 + heads(b)
      case Prog.Bind(m, rest) => heads(m) + heads(rest)
      case Prog.Reset(_, b) => heads(b)
      case Prog.Dollar(_, _, b) => heads(b)
      case Prog.Shift0(_, b) => bodyHeads(b)
      case Prog.Shift(_, b) => bodyHeads(b)
      case _ => 0
    def bodyHeads(b: Body): Int = b match
      case Body.Then(_, after) => heads(after)
      case Body.ResumeWith(m) => heads(m)
      case _ => 0
    assert(progs.count(heads(_) > 0) >= 500, "too few programs reach the door")
  }
