## optics-section-audit - four of five optics lanes were closed, and the board said none were

Bookkeeping, taken because the queue pointed here and the queue was
wrong. `optics-arrow-instances` was claimed to implement
`Arrow[Function1]` and the Kleisli — and found them already landed,
by a sibling, on the shared `arrow-laws` suite exactly as the plan
said ("one suite, two lanes, first to land writes it"). The BACKLOG
entry still read `- [ ]`.

CLOSED HERE, each under the condition it was written with:

- `optics-arrow-instances` — landed. Worth keeping from it: the
  Function1 arrow is a WIDENING of the instance that already existed,
  not a second given, because `Arrow` extends `Strong` and a second
  given would make `Strong[Function1]` ambiguous at every optic call
  site in the library.
- `optics-prism-selective` — closed by ITS OWN GATE rather than by
  being done. It said: take the `Star[F]` road "only if a consumer
  wants the applicative `Static` through a sum rather than `Proc`",
  and close it when static-workflow stage 3 lands and nobody has.
  Stage 3 landed; nobody has. The design stays in the entry, which
  costs nothing and is what a reopen would need.
- a DUPLICATE `optics-guide-page` entry — the done record and the
  original text had been split into two files by the boards migration
  (they shared a slug), and the done one already quotes the original.
- two entries that are RECORDS and not lanes now carry names that say
  so instead of `item-107` / `item-108`.

AND THE static-workflow QUEUE LINE IS GONE, as its own text asked: it
stayed one cycle so the next agent would not go looking for the
stages, and this is the next cycle.

What is left on the optics side is three genuinely open items, none
blocking anything, with `optics-field-fuse` carrying the instruction
that matters — measure the by-name lens before choosing what to do
about it, because it has no benchmark row of its own.
