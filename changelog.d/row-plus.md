## okay-cont: the row operator is `+:`, the empty row `Pure`

Lane row-plus (specs/freer-min.md, stage 38). A row of the machine reads
as the classic one with a colon: `State % Int +: Throws % String +: Pure`,
right-associative by its last character, a nominal list, every walk over
it total. Probed and refused: `+` itself as the nominal operator — being
left-associative it can only build a tree at the effects' kind, where a
join is itself an "effect" and `Has` of one effect keeps an arm the types
cannot exclude.
