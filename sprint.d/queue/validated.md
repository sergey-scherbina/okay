- validated — every error, not the first (specs/validated.md, stage 0
  the spec LANDED 2026-09-18). P13 ITEM 1 AND THE NEXT THING TO PICK:
  `Throws` is monadic and stops at the first error, an applicative
  cannot stop and therefore collects. `Validated[E, A]` with a
  `Semigroup[E]`, no `Monad` instance on purpose (the consistency law
  would force the short-circuit the type exists to refuse), and every
  generic combinator already written against `Applicative` works at it
  the day the instance exists. First real consumer: `okay-conf`
  reporting every missing key in one run, which is also the item that
  decides whether it earned its place.
