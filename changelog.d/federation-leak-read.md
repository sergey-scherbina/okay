## federation-leak-read - Claim 3, made mechanical: what a job's partial actually reveals

Stage 4 of specs/federation.md asked for a tool that "prints what a
job's partial Schema lets out, per key, and flags a count that can be
one" — Claim 3 restated as something you can run rather than argue
about: "an aggregate can still be a record. A count of one, a max over
one element, a key that identifies a person — these are records with
a different name."

`Leak.of(wire: Schema[?])` walks a `Wire#wire` — the exact Schema every
partial that crosses a wire is described by — and reports every field
that crosses, per key where there is one and per PARTITION where there
is not (a `Wire.fold` accumulator has no key at all, and asks the same
question at that coarser grain).

TWO FLAGS, EACH MECHANICAL AND NEITHER A VERDICT. A KEY whose shape has
no bound on cardinality (`String`, raw `Bytes`) is UNBOUNDED — an email
address and a two-letter country code have the identical shape, so the
Schema cannot tell them apart, which is exactly why this is a thing to
read rather than something the tool decides for you. A VALUE that is a
bare scalar with nothing beside it to say how many records made it is
UNCOUNTED — a group of one, or a fold over a partition of one event,
is then indistinguishable on the wire from that record's own field.

PROVEN AGAINST THIS REPOSITORY'S OWN SHAPES, not invented ones.
`TestJobs.value` — a bare `Long` per key, used throughout okay-cluster's
own test suite — is uncounted. `PartyJob`'s own partial, the actual job
`TestFederation` federates over, is uncounted for the identical reason.
Neither was built to make a point; both were already here.

A COMPOUND VALUE IS NAMED AND LEFT THERE, on purpose. The first cut of
this tool recursed into every product it found, and `Feeds.Sum(n,
total, x)` used as a windowed sink's terminal accumulator came out as
THREE separately-flagged `Long`s (`finished.n`, `finished.total`,
`finished.x`), each "uncounted" — which buries the one finding that
matters (a bare scalar with nothing beside it) under noise from an
accumulator that already carries its own explanation. The fix: `walk`
only opens a product when it is one of `okay.cluster.Wire`'s own
composition envelopes (`pair`/`triple`/`handed` — the literal names
those functions give their `SProduct`s); every other product is a
user's own accumulator and is reported as ONE field, by name. Whether
its own fields answer "how many" is a question about what they MEAN,
which is not a Schema's business — the tool stops at the shape, as the
spec's own wording asks ("read, reasoned about").

Both flags have a negative control: removing the envelope check
re-fragments `Sum` and two tests fail; removing the uncounted
computation entirely drops three assertions across the fold, keyed and
windowed shapes. 9 tests, 127 green in okay-cluster.
