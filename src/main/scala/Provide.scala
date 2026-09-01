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
 *
 * Arities 1..22, GENERATED (the Cats mapN answer, applied: the
 * "unbounded" idiom is 22 fixed overloads, the platform's own cap —
 * ContextFunctionN ends at 22). The single-definition tuple route
 * is blocked by param-position match-type behavior, recorded as
 * E11/E12 in the spec; regenerate with tools/gen_provide.py.
 */

inline def provide[A1, B](a1: A1)(inline body: A1 ?=> B): B =
  body(using a1)

inline def provide[A1, A2, B](a1: A1, a2: A2)(inline body: (A1, A2) ?=> B): B =
  body(using a1, a2)

inline def provide[A1, A2, A3, B](a1: A1, a2: A2, a3: A3)(inline body: (A1, A2, A3) ?=> B): B =
  body(using a1, a2, a3)

inline def provide[A1, A2, A3, A4, B](a1: A1, a2: A2, a3: A3, a4: A4)(inline body: (A1, A2, A3, A4) ?=> B): B =
  body(using a1, a2, a3, a4)

inline def provide[A1, A2, A3, A4, A5, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5)(inline body: (A1, A2, A3, A4, A5) ?=> B): B =
  body(using a1, a2, a3, a4, a5)

inline def provide[A1, A2, A3, A4, A5, A6, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(inline body: (A1, A2, A3, A4, A5, A6) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6)

inline def provide[A1, A2, A3, A4, A5, A6, A7, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7)(inline body: (A1, A2, A3, A4, A5, A6, A7) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20, a21: A21)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

inline def provide[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, B](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20, a21: A21, a22: A22)(inline body: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) ?=> B): B =
  body(using a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
