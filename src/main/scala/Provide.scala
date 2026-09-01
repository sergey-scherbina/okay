package okay

/**
 * The installer half of the capability pair
 * (specs/context-functions.md, ctx-everywhere): expression-scoped
 * installation of givens — no given-line, no block nesting. With
 * the doors (APIs accepting `A ?=>`), this is the
 * dependency-injection story: compile-time resolution, given-scopes
 * as the object graph, zero framework. `provide(testEnv) { app }`
 * swaps a whole environment for a block.
 *
 * Nesting resolves to the NEAREST installation — inner provide
 * shadows outer, the E8-verified rule.
 */
inline def provide[A, B](a: A)(inline body: A ?=> B): B =
  body(using a)

inline def provide[A, B, C](a: A, b: B)(inline body: (A, B) ?=> C): C =
  body(using a, b)

inline def provide[A, B, C, D](a: A, b: B, c: C)(inline body: (A, B, C) ?=> D): D =
  body(using a, b, c)
