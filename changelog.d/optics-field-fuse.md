## optics-field-fuse - measured, and the answer is a price rather than a planner

`Lens.field[S]("name")` is the one constructor `Fuse` cannot read, so
the verdict table said it pays the interpreter. The entry asked for a
measurement before choosing between teaching the planner its shape and
saying the price out loud. Measured (`OpticsBenchmark`, per-lane
minima across three forks, `-prof gc`):

    copy by hand                     1.44 ns   24 B/op
    Lens[S](_.age) live              1.45 ns   24 B/op
    the same, fused                  1.45 ns   24 B/op
    Mirror.fromProduct alone         2.30 ns   40 B/op
    Lens.field fused                 3.96 ns   56 B/op
    Lens.field live                  5.69 ns   56 B/op

**THE FLOOR IS 40 B/op, NOT 24, AND THE OPTIC IS NOT WHY.** The middle
row is the new one and it decides the lane: rebuilding a case class
through `Mirror.fromProduct(Replaced(...))` costs 40 bytes with NO
optic above it, where `copy` costs 24. So the by-name road can never
reach the selector road however well it fuses, and everything a
planner could win is the 16 bytes the optic adds on top of the Mirror.

AND IT CANNOT BE WRITTEN AT A `Fuse.set` CALL SITE AT ALL, which the
entry did not know: `Fuse.set(Lens.field[Person]("age"))` does not
type-check. The focus type comes from a `Mirror` in the constructor's
own using-clause, and as an argument to an inline method with an
expected `Optic[...]` type that inference collapses to `Nothing`. The
only road is through a `val`, which is the road real code takes — so
the benchmark measures that one.

DECIDED: no planner work. The price goes on the guide page
(`docs/optics.md`, "What the by-name lens costs") with the rule it
implies — use `Lens[S](_.f)` wherever the name is known when the code
is written, which is nearly always, and keep `Lens.field` for a name
that arrives as data.

ONE THING NOT CLAIMED. `fieldSet` and `fusedFieldSet` differ by 1.4x
in TIME with identical BYTES, which is what a fallback should not do;
this repository's own rule is that the bytes are the proof and a time
gap without an allocation gap is a lane difference, not a finding. It
is left as measured rather than explained.

Two benchmark rows kept: `fusedFieldSet` and `mirrorSet` (the floor).
