- [ ] optics-arrow-instances — `Arrow[Function1]` and
      `Arrow[[A, B] =>> A => B ! R]`, so `split`/`fanout` exist on
      plain and effectful functions and the "one Profunctor" sentence
      in Optic.scala is a fact. Deliverable is the laws' tests
      (`TestMealy` states them over an input), not a capability: with
      a monad in hand `fanout` adds little over a for-comprehension,
      and the entry says so before anyone measures it.
      RE-CHECKED 2026-09-18: specs/static-workflow.md stage 1 wants
      the same category/arrow/choice laws as a property at `Proc`.
      ONE law suite parameterised by the carrier and an observation
      (`TestMealy` observes over an input) serves both; whichever
      lane lands first writes it reusable, the other reuses it.
      Neither lane adds the other's instance (that spec's Design).
