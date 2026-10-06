package okay.freer

/** a delimiter's identity: by allocation, compared by `eq`; `S` its answer index, `Y` its value; labelled for
 * diagnostics */
final class Prompt[S, Y](val label: String):
  override def toString: String = label

/**
 * The control signature, over the row `G` its bodies are written in: λ$'s two operations (Materzok–Biernacki,
 * shift0 and $), typed by Danvy–Filinski's answer-type modification — the typing of specs/freer-kont.md. Prompts
 * are NOT in the tree: these are operations like any other, and the machine that answers them is their handler.
 * A captured `k` is an ordinary function; the machine's stack implements it.
 */
enum Control[+G[_, _, +_], S, R, +A]:
  /** `ret $_p body`: delimit at `p`, `ret` the frame first above it (`reset p body = Return(_) $_p body`). Typed
   * as `Bind(body, ret)` is — the delimiter sits between the two */
  case Reset[G[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], ret: X => Freer[G, S, T, Y], body: Freer[G, T, R, X])
    extends Control[G, S, R, Y]

  /** capture up to `p`, the delimiter included: `k` is the segment as a function, `ret`'s shape; `f(k)` goes on in
   * the delimiter's place */
  case Shift0[G[_, _, +_], S, T, R, X, Y](p: Prompt[S, Y], f: (X => Freer[G, S, T, Y]) => Freer[G, S, R, Y])
    extends Control[G, T, R, X]

/** `Control` over a row, as a row */
type Ctl[G[_, _, +_]] = [S, R, A] =>> Control[G, S, R, A]
