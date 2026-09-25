## book-stacks-do-not-compose - chapter 16b: transformer stacks do not compose with each other

docs/continuations/16b-two-monads-at-once.md, a new subsection after
the cats basket: three teams with the same three effects in three stacks.

- Team B's helper, written for the other ORDER (`WriterT[EitherT[List]]`),
  does not type in team A's `EitherT[WriterT[List]]`; the compiler's own
  refusal is quoted and pinned by a `compileErrors` test.
- Team C's stack with one more layer (`ReaderT` on top) refuses team A's
  helper until it is lifted again (`priced`).
- A hand-written `reorder` between the two orders compiles and LOSES the
  log: in team B's order the error discarded it before the conversion
  runs. Pinned by a test next to team A's own order keeping the line.
- The contrast: an effect helper written once against `Writer % String`
  used unchanged in a bigger row (`Reader % Int + Basket`), tested; a
  layered helper is a plain value.
- TestBookTwoMonadsCats 4 -> 8.
