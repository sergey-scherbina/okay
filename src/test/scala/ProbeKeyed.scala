package okay

import okay.RowLift.{at, plus}

/**
 * PROBE: can a row hold TWO states? Yes — if the operation carries
 * something the runtime can see.
 *
 * `State`'s `Get()` carries no trace of S, so the class is its whole
 * identity and two `State % S` in one row misroute (TestRowIdentity
 * demonstrates it). `Writer` escapes that without trying, because its
 * operation IS the value told: telling a String is a String at run
 * time. That is the principle, and everything else follows from it —
 * the split is a runtime test, so identity has to exist at run time.
 *
 * Three ways to give it one, in increasing cost:
 *
 *   a KEY the operation holds (this probe) — one field, and the test
 *   compares it; a row may then hold as many states as it has keys
 *
 *   a TAG for S itself (a ClassTag or a derived type tag) — no key to
 *   invent, but erasure-equal types (List[Int], List[String]) stay
 *   indistinguishable, so it fixes less than it looks
 *
 *   a scoped PROMPT (Delim already has multi-prompt control) — the
 *   handler installs a fresh identity per installation, so even two
 *   states of the SAME type are distinct. The most correct and the
 *   most invasive: the program has to carry the prompt.
 *
 * Measured here: the key route works end to end. `KEYED (8,(ada!,...))`
 * — the count went 7 -> 8 and the name "ada" -> "ada!", each reaching
 * its own handler through one row.
 *
 * Not proposed for the library as it stands: `KState` duplicates
 * `State` rather than extending it, and the handler below keeps its
 * cell in a `var` (a probe's shortcut — the real one threads it, as
 * `State.handle` does). What the probe is for is the CLAIM: the
 * limitation is not inherent to rows, it is inherent to operations
 * that carry nothing.
 */
object ProbeKeyed:

  /** state under a KEY the operation carries at run time */
  enum KState[K <: String & Singleton, S, +A]:
    case Get(k: K) extends KState[K, S, S]
    case Put(k: K, s: S) extends KState[K, S, S]

  /** the test is by key, not by class — that is the whole point */
  given keyed[K <: String & Singleton, S](using k: ValueOf[K])
  : TypeableK[[A] =>> KState[K, S, A]] = new:
    def unapply[A](x: Any): Option[x.type & KState[K, S, A]] = x match
      case op: KState[?, ?, ?] =>
        val same = op match
          case KState.Get(kk) => kk == k.value
          case KState.Put(kk, _) => kk == k.value
        if same then Some(x.asInstanceOf[x.type & KState[K, S, A]]) else None
      case _ => None

  type At[K <: String & Singleton, S] = [A] =>> KState[K, S, A]

  def get[K <: String & Singleton, S](using k: ValueOf[K]): S ! At[K, S] =
    effect(KState.Get[K, S, S](k.value))
  def put[K <: String & Singleton, S](s: S)(using k: ValueOf[K]): S ! At[K, S] =
    effect(KState.Put[K, S, S](k.value, s))

  /** the handler for ONE key, forwarding everything else */
  def run[K <: String & Singleton, S, A, F[+_]](init: S)(p: A ! (At[K, S] + F))
    (using TypeableK[At[K, S]]): (S, A) ! F =
    var cur = init
    !.relay[A, (S, A), At[K, S], F](p)(a => pure((cur, a))):
      [X, Y] => e => e match
        case KState.Get(_) => Cont.Pure(cur)
        case KState.Put(_, s) => cur = s.asInstanceOf[S]; Cont.Pure(cur)

  type Row = At["count", Int] + At["name", String]

  val prog: (Int, String) ! Row =
    for
      n <- get["count", Int].plus[At["name", String]]
      _ <- put["count", Int](n + 1).plus[At["name", String]]
      s <- get["name", String].at[Row]
      _ <- put["name", String](s + "!").at[Row]
      n2 <- get["count", Int].plus[At["name", String]]
      s2 <- get["name", String].at[Row]
    yield (n2, s2)

  def main(args: Array[String]): Unit =
    val inner = run["name", String, (Int, String), At["count", Int]]("ada")(
      prog.at[At["name", String] + At["count", Int]])
    println("KEYED " + !.run(run["count", Int, (String, (Int, String)), Pure](7)(inner)))
