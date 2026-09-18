package okay

/**
 * ZOOMING BY AN OPTIC — the half of `State` and `PState` that is
 * about optics, which is why it lives here and not in the core
 * (core-modules stage 4).
 *
 * The core keeps the INTERPRETATIONS, spelled in its own terms:
 * `State.zoomWith(look, put)` takes the two functions it actually
 * uses, and `PState.Zooming` is a `Cont` alias with no optic in it.
 * What is here is the optic SPELLING of the same things, given back
 * as extensions so that `State.zoom(lens)(prog)` and
 * `PState.zoom(lens)(m)` compile character for character wherever
 * okay-optics is on the classpath. Nothing any caller had written
 * changed.
 */

/** the lens spelling of `State.zoomWith`: a program written against a
 * PART of the state, run against the whole, with the lens saying
 * which part and nothing else touched */
def zoomLens[S, A, X, F[+_]](l: Lens[S, S, A, A])(p: X ! State % A + F): X ! State % S + F =
  State.zoomWith[S, A, X, F](s => l.get(s), a => s => l.set(a)(s))(p)

extension (st: State.type)
  /** see `zoomLens` — this is the name every caller already writes */
  def zoom[S, A, X, F[+_]](l: Lens[S, S, A, A])(p: X ! State % A + F): X ! State % S + F =
    zoomLens[S, A, X, F](l)(p)

extension (ps: PState.type)
  /**
   * A typestate program over a PART, run over the whole — and this is
   * the stage's whole argument (specs/optics.md stage 3, theory ch. 3).
   *
   * A four-parameter lens `Lens[S1, S2, A1, A2]` is a type-changing
   * update: the whole goes S1 -> S2 exactly when the part goes
   * A1 -> A2. `PState` is Atkey's parameterised state: a transition
   * that carries the state's TYPE in the answer type. Zooming one by
   * the other is one `shift` — read the part out of the whole to start
   * the inner program, and put the part back to finish it — and the
   * types line up on their own, which is the sense in which the
   * type-changing lens and parameterised state are the same picture.
   */
  inline def zoom[S1, S2, A1, A2, X, R](l: Lens[S1, S2, A1, A2])
                                       (m: Cont[X, A2 => R, A1 => R]): Cont[X, S2 => R, S1 => R] =
    l[PState.Zooming[X, R]](m)
  /**
   * A PRISM CANNOT BE AN INSTANCE HERE, AND THE REASON IS NOT THE
   * TYPES — it is that the program has no answer to give.
   *
   * `Choice.right` would have to turn a `P[A, B]` into a
   * `P[Either[C, A], Either[C, B]]`: a program that runs on the
   * `Right` and passes a `Left` through. On the `Right` that is
   * ordinary. On the `Left` the zoomed program must still produce the
   * answer `X` — and `X` is universally quantified in this instance,
   * so there is no `X` to produce and no continuation to get one
   * from. It is a parametricity argument, not a compiler complaint:
   * the only source of an `X` is the inner program, and the inner
   * program is exactly what the absent case says not to run.
   *
   * So the honest door is the one below, and what it costs is written
   * in its type: the answer becomes `Option[X]`. `TestZoomPrism` pins
   * that the instance is absent (a refusal is the only thing that can
   * prove it) and that this door does what a prism should.
   */
  def zoomCase[S1, S2, A1, A2, X, R](p: Prism[S1, S2, A1, A2])
                                    (m: Cont[X, A2 => R, A1 => R]): Cont[Option[X], S2 => R, S1 => R] =
    // the prism's own pair, taken by running it at `Market` — the
    // representation `Optic.compiled` exists for exactly this: to hand
    // an optic's two halves to something that is not a profunctor
    val pair = p.compiled
    shift(k => (s1: S1) => pair.look(s1) match
      case Right(a1) => (m / (x => (a2: A2) => k(Some(x))(pair.put(s1, a2))))(a1)
      // the case is not there: the program never runs, the state is
      // already the `S2` the prism found, and the answer says so
      case Left(s2) => k(None)(s2))

  /** the `Strong` instance for the zooming carrier, by name */
  def strong[X, R]: Optic.Strong[PState.Zooming[X, R]] = opticZooming[X, R]

/**
 * `Strong`, and why `first` is the whole content: the state is a
 * PAIR, the program works on its left half, and the right half
 * rides along untouched. That is exactly what a lens does to a
 * record, which is why `first` and `lens` are the same shape here.
 *
 * `lens` is overridden rather than derived so that no tuple is
 * built per zoom — the direct road every interpretation in
 * Optic.scala is allowed to take, and here it is the body `zoom`
 * had before this instance existed, character for character.
 */
private[okay] class ZoomStrong[X, R] extends Optic.Strong[PState.Zooming[X, R]]:
  def dimap[A, B, C, D](p: Cont[X, B => R, A => R])(f: C => A, g: B => D): Cont[X, D => R, C => R] =
    shift(k => (c: C) => (p / (x => (b: B) => k(x)(g(b))))(f(c)))

  def first[A, B, C](p: Cont[X, B => R, A => R]): Cont[X, ((B, C)) => R, ((A, C)) => R] =
    shift(k => (ac: (A, C)) => (p / (x => (b: B) => k(x)((b, ac._2))))(ac._1))

  override def lens[S1, S2, A1, A2](get: S1 => A1, set: (S1, A2) => S2)
                                   (p: Cont[X, A2 => R, A1 => R]): Cont[X, S2 => R, S1 => R] =
    shift(k => (s1: S1) => (p / (x => (a2: A2) => k(x)(set(s1, a2))))(get(s1)))

/**
 * The zooming carrier's `Strong`, TOP-LEVEL so that `import
 * okay.given` finds it — the placement every interpretation in
 * Optic.scala uses, and for the reason the compiler gave when this
 * one was written inside `object PState`: the implicit scope of
 * `Cont[X, B => R, A => R]` is `Cont`'s, not `PState`'s, so a user
 * zooming by hand would have needed an import nobody could guess.
 * `PState.zoom` never noticed, because it is lexically inside.
 */
given opticZooming[X, R]: Optic.Strong[PState.Zooming[X, R]] = ZoomStrong[X, R]()
