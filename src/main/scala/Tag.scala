package okay

import okay.RowLift.plus

/**
 * ANY effect, under a key — so a row may hold several instances of
 * one signature, and each is handled on its own.
 *
 * `Keyed` did this for `State`. Nothing in it was about state: a row
 * is split by a RUNTIME test, so two members of the same signature are
 * told apart exactly when the operation carries something the test can
 * compare. Wrapping any operation in a key is that something, once,
 * for every effect there is.
 *
 *     type Small = Tag.Of["small", State % Int]
 *     type Big   = Tag.Of["big",   State % Int]
 *
 * TAG A PROGRAM, NOT AN OPERATION. `tag` walks a finished program and
 * puts every F operation under the key, which is what makes this worth
 * having: a function written against a plain `State % Int` — anyone's
 * function, already compiled — can be run twice in one program at two
 * different states, without being written for it.
 *
 *     val twice: (Int, Int) ! (Small + Big) =
 *       for
 *         a <- Tag.tag["small", State % Int](count).plus[Big]
 *         b <- Tag.tag["big",   State % Int][Int, Pure](count).at[Small + Big]
 *
 *     The second line writes the second type clause because `G` is
 *     inferred from the program, and a program of `State` alone infers
 *     `G = State % Int` rather than `Pure` — which `.plus` accepts and
 *     `.at` does not (generalized-method-syntax, 2026-09-11).
 *       yield (a, b)
 *
 * HANDLING IS THE EFFECT'S OWN. There are no new handlers to write:
 * `untag` strips one key and hands back the plain signature, which the
 * effect's existing handler then takes.
 *
 *     State.run(0)(Tag.untag["small", State % Int](twice))   // and so on
 *
 * THE TEST IS THE KEY AND THE SIGNATURE (tag-test-the-signature-too,
 * 2026-09-11). It was the key alone, and then two members sharing a
 * key collided however different their effects were. Asking
 * `TypeableK[F]` as well — which `Instances` does, and which cost
 * NOTHING to require, since an `Effect` IS a `TypeableK` and every
 * effect that goes under a key already has one — makes `Of["k", Beep]
 * + Of["k", Buzz]` an ordinary row.
 *
 * WHAT NO RUNTIME TEST CAN FIX, and what therefore stays your rule to
 * keep: the same SIGNATURE under the same key. `Of["same", Reader %
 * Int] + Of["same", Reader % String]` still misroutes into the
 * ClassCastException a key exists to prevent, because `Int` and
 * `String` were erased before the test could see them and the key is
 * the same. `TestTag` pins both halves, docs/many-instances.md states
 * the rule, and `tag-distinct-keys` in BACKLOG.md is the compile-time
 * check that would make stating it unnecessary.
 *
 * AND THE OTHER TWO ROUTES. A key is a STATIC identity: the row lists
 * the instances, so the compiler knows how many there are and nothing
 * casts. `Refs` is the dynamic counterpart for state made at run
 * time, at the price of a heap and one cast. The third is the one
 * `Delim` already offers — a fresh PROMPT per handler installation,
 * which gives an instance an identity that no type has to name and no
 * key has to be invented for; it is the most scoped of the three, and
 * the most invasive, since the program carries the prompt. Use a key
 * when the instances can be named, cells when they are made, a prompt
 * when they must be nested and separated dynamically.
 */
final case class Tag[K, F[+_], +A](key: K, op: F[A])

object Tag:

  /** one instance of F, named — a row member */
  type Of[K, F[+_]] = [A] =>> Tag[K, F, A]

  /** the test is the KEY and the SIGNATURE; the type ARGUMENT under
    * that signature is erased and no test can reach it */
  given of[K, F[+_]](using k: ValueOf[K], t: TypeableK[F]): okay.Effect[Of[K, F]] =
    okay.Effect.of(new:
      def unapply[A](x: Any): Option[x.type & Tag[K, F, A]] = x match
        case w: Tag[?, ?, ?] if w.key == k.value && t.test(w.op) =>
          Some(x.asInstanceOf[x.type & Tag[K, F, A]])
        case _ => None)

  /** perform one operation under the key */
  inline def one[K, F[+_]](using k: ValueOf[K])[A](op: F[A]): A ! Of[K, F] =
    effect(Tag[K, F, A](k.value, op))

  /** put every F operation of a program under the key, leaving the
   * rest of the row alone */
  def tag[K, F[+_]](using TypeableK[F], ValueOf[K])[A, G[+_]]
                   (p: A ! (F + G)): A ! (Of[K, F] + G) =
    !.interpret[A, F, Of[K, F], G](p)([X] => (e: F[X]) => one[K, F](e).plus[G])

  /** strip one key, handing back the plain signature for its own
   * handler to take */
  def untag[K, F[+_]](using TypeableK[Of[K, F]])[A, G[+_]]
                     (p: A ! (Of[K, F] + G)): A ! (F + G) =
    !.interpret[A, Of[K, F], F, G](p)([X] => (e: Tag[K, F, X]) => effect[F + G, X](e.op))

  /** a comonadic handler for one key, out of the effect's own */
  def handler[K, F[+_]](h: Handler[F]): Handler[Of[K, F]] = new:
    def handle[A](e: Tag[K, F, A]): A = h.handle(e.op)
