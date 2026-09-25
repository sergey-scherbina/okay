## stack-safety-json - the strict JSON reader skips unknown fields without the stack

This is stage 2a of specs/stack-safety.md: the JSON family in both
cores.

The defect: past `Codecs.NativeThreshold` the strict reader
(`JsonStrict`) walks on `Cont`. But it skipped each unknown field by a
direct call back into its field loop, one frame per skipped field. An
object 40 levels deep carrying 200 000 fields the schema does not name
threw StackOverflowError, in okay and in okay2 alike. The test in each
core's TestJsonStrict failed first. Now the fields up to the next known
one are consumed by a loop, and a known field still descends through
`Cont.defer`.

`recscan` over both codec modules no longer reports the two methods.
Every other JSON row in `specs/stack-safety-okay.tsv` and
`specs/stack-safety-okay2.tsv` is marked BOUNDED with its bound:
- one open container per call below the threshold, `Cont` past it;
- the product's field count for the absent-field step;
- the schema for an Option or iso chain between two containers.

The bound is written beside `JsonStrict.get` in both cores.
