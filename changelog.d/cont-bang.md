## okay-cont: `A ! R`, the machine's program in the classic spelling

Lane cont-bang (specs/freer-min.md, stage 41). `infix type ![A, R <: Row]
= Free[R, A]` and `%` for a parameterised effect in `okay.cont`, so a
program of the machine reads `Int ! (State % Int +: Say +: Pure)`;
`effect(op)` makes one of an operation, `p.value` runs a closed one.
