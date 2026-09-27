## fold-each: `!.foldEach`, a fold whose step is one bind by construction

`Effects.foldEach(xs)(z)(x => prog(x))(combine)`: the element's program
and a pure combine, so no map node is built and then thrown away. At
N = 1000, on top of map-cost-residual's in-place foldM: 143.7 KB/op
against foldM's 175.7 KB (-18%, 32 B an element). The time is the same,
18.1 against 18.3 µs/op, within the error. The hand-written one-bind
ceiling (10.0 µs) is not reached; the gap is the boxed accumulator and
the generic calls (specs/map-fusion.md). Tested in TestBuildShape; the
`stateFoldEach` JMH lane.
