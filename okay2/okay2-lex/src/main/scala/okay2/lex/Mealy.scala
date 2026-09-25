package okay2.lex

import okay2.Optic

/**
 * A Mealy machine: one input, one output, a new machine — okay-lex's
 * Mealy.scala. `Scan` is one of these with the state written out as a
 * value (fast, incremental); this is the same thing written so it
 * COMPOSES: a Mealy machine is an arrow — Category, Strong, Choice. A
 * step allocates the next machine, so this is the composition layer, not
 * the lexing road; `ofScan` is the door between.
 */
final case class Mealy[-A, +B](run: A => (Mealy[A, B], B))

object Mealy {

  /** the arrow, with the choice half beside it (an ArrowChoice) */
  implicit val mealyArrow: Optic.Arrow[Mealy] with Optic.Choice[Mealy] =
    new Optic.Arrow[Mealy] with Optic.Choice[Mealy] {
      def dimap[A, B, C, D](p: Mealy[A, B])(f: C => A, g: B => D): Mealy[C, D] =
        Mealy(c => { val (m, b) = p.run(f(c)); (dimap(m)(f, g), g(b)) })

      def first[A, B, C](p: Mealy[A, B]): Mealy[(A, C), (B, C)] =
        Mealy((ac: (A, C)) => { val (m, b) = p.run(ac._1); (first[A, B, C](m), (b, ac._2)) })

      def right[A, B, C](p: Mealy[A, B]): Mealy[Either[C, A], Either[C, B]] =
        Mealy {
          case Left(c) => (right[A, B, C](p), Left(c))
          case Right(a) => val (m, b) = p.run(a); (right[A, B, C](m), Right(b))
        }

      def arr[A, B](f: A => B): Mealy[A, B] = {
        lazy val m: Mealy[A, B] = Mealy(a => (m, f(a)))
        m
      }

      def compose[A, B, C](g: Mealy[B, C], f: Mealy[A, B]): Mealy[A, C] =
        Mealy { a =>
          val (f2, b) = f.run(a)
          val (g2, c) = g.run(b)
          (compose(g2, f2), c)
        }
    }

  /** a scanner as a machine: the state stays a value inside */
  def ofScan[K, S](sc: Scan[K, S]): Mealy[Char, Vector[Token[K]]] = {
    def at(s: S): Mealy[Char, Vector[Token[K]]] =
      Mealy(c => { val (s2, ts) = sc.step(s, c); (at(s2), ts) })
    at(sc.init)
  }

  /** run it over an input, keeping every answer */
  def runAll[A, B](m: Mealy[A, B], as: IterableOnce[A]): Vector[B] = {
    val out = Vector.newBuilder[B]
    var cur = m
    val it = as.iterator
    while (it.hasNext) { val (next, b) = cur.run(it.next()); out += b; cur = next }
    out.result()
  }

  /** run it over an input, FOLDING the answers */
  def fold[A, B, R](m: Mealy[A, B], as: IterableOnce[A])(z: R)(f: (R, B) => R): R = {
    var acc = z
    var cur = m
    val it = as.iterator
    while (it.hasNext) { val (next, b) = cur.run(it.next()); acc = f(acc, b); cur = next }
    acc
  }

  /** run it over the characters of a string */
  def runString[B](m: Mealy[Char, B], input: String): Vector[B] = runAll(m, input)
}
