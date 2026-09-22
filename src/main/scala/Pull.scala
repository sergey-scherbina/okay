package okay

/**
 * A SOURCE A PROGRAM READS ONE STEP AT A TIME (specs/direct-loops.md,
 * v3). `step` is the next element and the rest — or None — as a
 * program in `G`: a `Stream[S, G]` carrier's `uncons`, a writer
 * program's next told value, the `Take` side of a `Stage` (`await`).
 * It is what `for x <- src do body` inside a `direct` block loops
 * over when `src` has no iterator to read: the loop is emitted as a
 * program that binds one `step` per element. Outside a block the
 * loop is `loop(f)` by name, a program.
 *
 * A type of its own rather than a road every `Stream` carrier takes
 * silently: `s.foreach(f)` on a carrier already means "run it through
 * the Handler here" (Stream.scala), and one spelling must not mean
 * two things depending on where it stands.
 */
trait Pull[A, G[+_]]:
  /** the next element and the rest, or None at the end — in `G` */
  def step: Option[(A, Pull[A, G])] ! G

  /** the source with the elements `p` refuses skipped — what a guard
   * `for x <- src if p(x) do …` desugars to; inside a block the macro
   * peels it off and tests per element, and outside one this is the
   * program that does the same */
  def withFilter(p: A => Boolean): Pull[A, G] =
    val self = this
    new Pull[A, G]:
      def step: Option[(A, Pull[A, G])] ! G =
        !.loop[Pull[A, G], Option[(A, Pull[A, G])], G](self) { s =>
          s.step.map {
            case Some((a, next)) if p(a) => Right(Some((a, next.withFilter(p))))
            case Some((_, next)) => Left(next)
            case None => Right(None)
          }
        }

  /** the loop with a pure body, AS A PROGRAM — the form written by
   * name outside a block. Inside a `direct` block `for x <- src do
   * body` is `Direct`'s `foreach` extension on this type, typed Unit
   * so the statement is not a discarded program, and rewritten into
   * this loop by the macro; outside one that `for` does not typecheck
   * at all, on purpose — a loop that is a program must be run, and
   * this is its name */
  def loop(f: A => Unit): Unit ! G =
    !.loop[Pull[A, G], Unit, G](this) { p =>
      p.step.map {
        case Some((a, next)) => f(a); Left(next)
        case None => Right(())
      }
    }

object Pull:
  /** any `Stream` carrier, read by its `uncons` */
  def of[S[_], A, G[+_]](s: S[A])(using St: Stream[S, G]): Pull[A, G] = new:
    def step: Option[(A, Pull[A, G])] ! G = St.uncons(s).map(_.map((a, rest) => (a, of(rest))))

  /** the told values of a writer program — first-order, as
   * Stream.scala's own overloads are: inference does not reach the
   * writer carrier's `Stream` instance through its type lambda */
  def told[W, A](a: A ! Writer % W): Pull[W, Pure] =
    of[[X] =>> A ! Writer % X, W, Pure](a)(using feedStream[A])

  /** the same, the producer performing `G` between tells: `G` is
   * where the reader's steps run */
  def toldIn[W, G[+_] : TypeableK, A](a: A ! Writer % W + G): Pull[W, G] =
    of[[X] =>> A ! Writer % X + G, W, G](a)(using writerStreamIn[A, G])
