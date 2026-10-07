## okay-cont: a row as the classic union; the same effects in another order are one program

Lane row-union (specs/freer-min.md, stage 40). `Union[R, X]` maps a
nominal row to the union of its operations, `Union[A +: B +: Pure, X]`
is `A[X] | B[X]` — commutative, so rows with the same effects in any order
have one union. `Free.reordered`, a conversion in `Free`'s companion:
where a program over `R2` is expected and one over `R1` given, the compiler
widens it by `Sub` when their unions are one (at `Any`); a row with fewer
effects is still a written `widen`.
