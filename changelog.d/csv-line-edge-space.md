## csv-line-edge-space - Csv.line quotes an edge space, and a null is an empty field

Found from okay-watch (its one CSV writer, `Sheet`, was `Csv.line` plus this
rule): a field with a leading or trailing space was written unquoted, and a
spreadsheet trims it — the value shown is not the value `Csv.fields` reads.

- `Csv.line` quotes such a field; a null field is an empty one. Still the
  inverse of `Csv.fields` (TestBulk).
- Gate: `affected master` 5329 GREEN, no warnings (a first run red on
  okay-reactive's TCK spec105 timing — filed reactive-tck-spec105-timing;
  the suite alone green).

Landed as e06cf4919.
