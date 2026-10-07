## okay-cont: `+` beside `+:`; a row program measured

Lane cont-row-target (specs/freer-min.md, stages 43–44). `Pure + State %
Int + Say` is the row `Say +: State % Int +: Pure`: `+` adds an effect to
the row on its left (`infix type +[R <: Row, E[+_]] = E +: R`, no match
type, so a rest `F + A` reduces), `+:` puts one in front of the row on its
right; both one list, the order of effects saying nothing. Measured: a row
program at 24 and 32 ns an operation against the context form's 6.7 and
16.5 — the cost is the rows joined at every bind (`Shape.split`), the
fixed-row design proposed; lanes `rowAnswering`, `rowGeneral`.
