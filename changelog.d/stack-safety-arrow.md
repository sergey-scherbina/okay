## stack-safety-arrow - okay-arrow refuses a type nested past Arrow's own limit of 64; an empty Arrow Java list converts

The stage-9 guard's first run named fourteen stack recursions in
okay-arrow. The one that mattered was the stream reader's `parseField`,
which recursed once per level of the SCHEMA it read. A stream whose schema
nests 100 000 lists overflowed on a 256 KB stack (`TestArrowDepth`, red
first); the schema is input a writer chose.

It is fixed by a bound, because Arrow's reference implementation has one:
C++ `IpcReadOptions::max_recursion_depth` (`kMaxNestingDepth` = 64).
`Column.MaxNesting` = 64, with `Column.nesting` counted on an explicit
stack, is checked at every door a type comes in by:

- the schema in `OkayArrow.parseField`, before it descends further;
- `Table`'s constructor, which every written column and every read
  result passes through;
- Arrow Java's schema in `ApacheArrow.fromRoot`.

Each is refused by name. `TestArrowNesting` (all platforms) round-trips a
column nested exactly 64 deep and refuses 65 at the Table and in a
schema. The fourteen walks behind those doors are BOUNDED rows now.

Found beside it: `ApacheArrow.fromRoot` threw `IndexOutOfBoundsException`
on any EMPTY root with a list column, because Arrow Java allocates a
list's offsets buffer with its first value and the conversion read
offset 0 from nothing. An empty list vector's offsets are `Array(0)` now,
tested at 0, 1, 2 and 64 levels.
