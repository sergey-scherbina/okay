## biernacki-literature-fix - two backlog entries corrected against the papers' text

biernacki-literature filed two entries from paper titles and abstracts.
Reading the text corrected both.

- `monadic-reflection-stacked` cited "A reflection on
  continuation-composing style" (FSCD 2020) as its source. That paper's
  reflection is a Galois reflection between direct style and CPS, not
  monadic reflection. The entry now cites Filinski, "Representing layered
  monads" (POPL 1999), with Materzok & Biernacki (APLAS 2012) for why
  shift0 is enough for the layering.
- `stacked-shift0` said the type system for `Below` "already exists".
  Only the rule's SHAPE does. ICFP 2011's stack of contexts is
  positional, and our prompts are named, so `Below` drops through `P`.
  The entry now quotes the shift0 rule and says what transfers and what
  does not.
