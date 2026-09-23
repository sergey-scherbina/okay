## okay-frege - Frege programs as okay programs, through a thin Frege monad

New module `okay-frege` (JVM, `org.frege-lang:frege` 3.25.153 — checked
alive: Maven release 2026-08-23, FFI work through 2026). `okay.frege.Prog`,
written in Frege (src/main/frege), is okay's freer tree in Frege —
`await`, `tell`, `perform op`, `liftIO io`, with Functor/Applicative/Monad
— and `Frege.stage`/`Frege.run[F]` walk it: every step an okay program
node, every operation under okay's handlers, every continuation a Frege
function. So `Choose` resumes a Frege program per branch, existing Frege
IO enters as one step by `liftIO`, and no thread is involved (0.27 µs per
step). Lazy IO — a pure `[a] -> [b]` fed thunks that pull from okay — was
the road not taken, for the iteratee paper's reasons.

A first cut ran Frege's own IO on a virtual thread as the continuation:
correct, but one-shot, ~10.5 µs per element, and needing a Cleaner for
abandoned stages; dropped on the operator's direction for the thin
monad. `project/Frege.scala` compiles `.fr` forked with `-target 17`
(javac would emit major 69 on this build's JDK 25); `Frege.before` puts
the library's classes into the jar — `products` alone did not.

Union rows are built (`Frege.Row.of[F] | Frege.Row.of[G]`): dotty does
not infer the sides of a union type lambda. Docs: docs/modules/okay-frege.md,
guide §5, theory ch. 7 (Peyton Jones & Wadler 1993; Kiselyov & Ishii 2015).
specs/frege.md.
