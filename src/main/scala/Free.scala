package okay

/**
 * THE FREER MONAD'S NAMES AT THE CORE'S DOOR. The monad is `okay.freer` (module okay-freer): `Freer`, the
 * indexed tree; `Free`, the effect tree at `Unary`; the bridge `Unary`; the direct marker `DirectCtx`. The
 * core is the library over it — handlers, rows, `Effects` — and every file of it, and every program written
 * with `import okay.*`, keeps the names: an alias each, the companions by a stable path, so `Free.Return(a)`
 * builds, `case Free.Bind(Free.Inject(e), k)` matches and `Freer.Return[G, R, A]` is a type, as before.
 * `okay-direct`'s macros name the symbols by their home, `okay.freer`.
 */
type Freer[G[_, _, +_], S, R, +A] = okay.freer.Freer[G, S, R, A]
val Freer: okay.freer.Freer.type = okay.freer.Freer
type Free[F[+_], +A] = okay.freer.Free[F, A]
val Free: okay.freer.Free.type = okay.freer.Free
type Unary[F[+_]] = okay.freer.Unary[F]
val Unary: okay.freer.Unary.type = okay.freer.Unary
type Diagonal[F[+_]] = okay.freer.Diagonal[F]
type DirectCtx[F[_]] = okay.freer.DirectCtx[F]

// level 1 (specs/shift-effect.md): at the package's top, so `p.handle` and `p.run` need no import beyond `okay.*`
/**
 * ONE handler taken off (handler-single-pass, specs/handler-single-pass.md): a STEPPED handler is registered
 * on the program's stack of handlers, walked once by whoever forces it; any other handler runs its own `run`
 * over the program, as it always has. The types were checked by the caller's `handle`.
 */
def handleOne[A, E[+_], I, O[_], N[_[+_]], F[+_]](p: A ! E + F, h: Handler.Full[E, I, O, N])
                                                 (using A <:< I, Distinct[E + F], N[F]): O[A] ! F =
  h match
    case s: Handler.Stepped[?, ?, ?] => HandleFrames.handled[A, O, F](p, s)
    case _ => h.run[A, F](p)

// level 1 (specs/shift-effect.md): in the companion, so `p.handle` and `p.run` need no import
extension [A, G[+_]](p: A ! G)
  /** take the handler's effect off the row: `F`, the rest of the row, is what remains */
  def handle[E[+_], I, O[_], N[_[+_]], F[+_]](h: Handler.Full[E, I, O, N])
                                             (using row: (A ! G) =:= (A ! E + F), ok: A <:< I, d: Distinct[E + F], n: N[F]): O[A] ! F =
    handleOne[A, E, I, O, N, F](row(p), h)

  /** two handlers, innermost first: `p.handle(State(5), Throws.either)` is `p.handle(State(5)).handle(Throws.either)` */
  def handle[Ef1[+_], I1, O1[_], N1[_[+_]], F1[+_], Ef2[+_], I2, O2[_], N2[_[+_]], F2[+_]](
      h1: Handler.Full[Ef1, I1, O1, N1], h2: Handler.Full[Ef2, I2, O2, N2])
      (using r1: (A ! G) =:= (A ! Ef1 + F1), ok1: A <:< I1, d1: Distinct[Ef1 + F1], n1: N1[F1],
             r2: (O1[A] ! F1) =:= (O1[A] ! Ef2 + F2), ok2: O1[A] <:< I2, d2: Distinct[Ef2 + F2], n2: N2[F2]): O2[O1[A]] ! F2 =
    handleOne[O1[A], Ef2, I2, O2, N2, F2](r2(handleOne[A, Ef1, I1, O1, N1, F1](r1(p), h1)), h2)

  /** three handlers, innermost first */
  def handle[Ef1[+_], I1, O1[_], N1[_[+_]], F1[+_], Ef2[+_], I2, O2[_], N2[_[+_]], F2[+_], Ef3[+_], I3, O3[_], N3[_[+_]], F3[+_]](
      h1: Handler.Full[Ef1, I1, O1, N1], h2: Handler.Full[Ef2, I2, O2, N2], h3: Handler.Full[Ef3, I3, O3, N3])
      (using r1: (A ! G) =:= (A ! Ef1 + F1), ok1: A <:< I1, d1: Distinct[Ef1 + F1], n1: N1[F1],
             r2: (O1[A] ! F1) =:= (O1[A] ! Ef2 + F2), ok2: O1[A] <:< I2, d2: Distinct[Ef2 + F2], n2: N2[F2],
             r3: (O2[O1[A]] ! F2) =:= (O2[O1[A]] ! Ef3 + F3), ok3: O2[O1[A]] <:< I3, d3: Distinct[Ef3 + F3], n3: N3[F3])
      : O3[O2[O1[A]]] ! F3 =
    handleOne[O2[O1[A]], Ef3, I3, O3, N3, F3](
      r3(handleOne[O1[A], Ef2, I2, O2, N2, F2](r2(handleOne[A, Ef1, I1, O1, N1, F1](r1(p), h1)), h2)), h3)

extension [A](p: A ! Pure)
  /** a program with no effect left, run to its value */
  inline def run: A = p.runWith
