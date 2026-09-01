# error-messages — the compile-error quality pass

## Overview

A user directive made obligatory (2026-09-02): compile errors must
say WHAT is wrong, WHY, and WHAT TO DO. Three instruments, in order
of leverage: `@implicitNotFound` on the typeclasses and capabilities
people actually miss (the "no given instance" wall becomes a
recipe); an audit of every macro/inline error to the what-why-fix
formula (the direct macro already leads here — its refusals name
workarounds); and compileErrors tests that assert MESSAGE QUALITY —
an actionable substring, not mere nonemptiness — so wording cannot
degrade silently. The limit is stated honestly: Scala 3 cannot
customize AMBIGUITY errors, only absence — ambiguities are fixed by
removing them (the .? retirement did exactly that), never by
wording.

## Interface

No API. Annotations and messages only.

## Behavior

- [x] a missing `Monad[F]` names the given import (`import
  okay.given`) and the program-monad case (`A ! Row` has its
  instance in Free's companion — check the row for Choose overlap)
- [x] a missing `Handler[F]` says what a handler is and points at
  the union builder for rows
- [x] a missing `TypeableK[F]` explains the total-split requirement
  and the one-class-carries-the-signature pattern (annotation only —
  unpinnable, see Results)
- [x] a missing `Direct.Effect[G]` says auto-coloring is opt-in and
  shows the one-line registration
- [x] a missing `DirectCtx[F]` says auto-coloring works only inside
  `direct { }`
- [x] a missing `CanBlock` names the door (`Blocking`) and the
  scheduler rule it protects (annotation only — the JVM default
  given always resolves in-package, see Results)
- [x] TestErrorMessages asserts an ACTIONABLE substring for each of
  the above and for the direct macro's standing refusals — the
  wording is pinned by test

## Out of scope

- Ambiguity texts (uncustomizable; fixed by removal, tracked per
  case — the mark family is already clean).
- Runtime exception messages (a separate audit; the condition
  system's Unhandled/NoSuchRestart already name menu and fix).

## Decisions

- **Pin what fails, annotate what may fail elsewhere** — probed
  in-package: TypeableK and CanBlock ALWAYS resolve inside okay
  (the derivation; the JVM default CanBlock), so their texts cannot
  be asserted by compileErrors here; the annotations stand for
  downstream scopes and the probe result is recorded so nobody
  writes the unpinnable test again.
- **The macro's messages were already the standard** — the audit
  found the direct refusals name position and workaround throughout;
  the pass pinned them rather than rewrote them.

## Results

- @implicitNotFound landed on Monad, Applicative, MonadPlus,
  Handler, TypeableK, CanBlock, Direct.Effect, DirectCtx — each
  message a recipe (the import, the union builder, the one-line
  registration, the door), not a lament.
- TestErrorMessages pins 5 wordings (Monad, Handler, Effect,
  DirectCtx, and both standing macro refusals); a rewrite that
  loses the actionable substring fails the suite.
- Probe finding: compileErrors snippets typecheck at the CALL SITE
  scope — extensions used in a snippet need their import IN the
  snippet string.
