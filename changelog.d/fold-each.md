## fold-each: `!.foldEach` — a fold whose step is one bind by construction

`Effects.foldEach(xs)(z)(x => prog(x))(combine)`: the element's program
and a pure combine, so each step is ONE bind. stateFoldEach 17.3 µs/op
against stateFoldM's 20.9 (1.21x), 143.7 KB/op against 191.7 KB (N = 1000).
It does not reach the hand-written one-bind ceiling (9.6 µs). The gap is
not bind count; it is recorded as open in specs/map-fusion.md. TestBuildShape
covers it, as does a `stateFoldEach` JMH lane.
