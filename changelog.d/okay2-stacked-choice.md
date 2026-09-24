## okay2-stacked-choice - the prompt stack as a type, Choose/Logic, SharedOnce

`Delim.Stacked` for the Scala 2 core: the stack of installed prompts
as an HList of prompt singleton types, `Has` evidence, a `Stack` value
whose methods are the doors (`shift`/`control`/`abort`/`reset`),
`In` and `delimited`; the three shapes that throw `NoPrompt` on the
plain doors are compile errors naming the stack, and the one hole the
missing `Prog` index leaves is pinned by a test. `Choose` with
`choose`/`fail`/`guard`/`runChoice` and `Logic` (msplit, cut, ifte,
gnot, interleave, fairBind, observe). `SharedOnce` in okay2-async: one
store for many fibres, an in-flight demand waits. The two Once
handler-order tests (inside the search: two runs; outside: one).
27 tests; 301 in the okay2 gate.

- The stack is a value and the doors are its methods: Scala 2 has no
  dependent function types and no `*:`, and a free-function door would
  need the stack's type as an explicit type argument.
- No `runSeq` (a collection is not a kind-`*` Row), no `withFilter`
  (no `Monad` here); `Choose.guard` is the step.
- `SharedOnce` walks with its own suspending loop at `Any`; a type
  variable in the pattern, not a wildcard, keeps `Store`'s two fields
  in agreement.

Docs: docs/okay2.md section 13; specs/okay2.md stage 7.
