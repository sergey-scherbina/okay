## shift-merge — Delim and Shift, one effect named Shift

Operator decision, 2026-10-02. `Delim` is folded into `Shift`: one effect
`Shift % K`, the key a type — the answer type (`reset`, the static form),
or `?` for prompts made at run time (the operator's glyph `Shift % ?`,
what `Delim` was). `type Delim` and `object Delim` are gone; every use in
109 files of 8 modules and 49 docs moved (okay2's own `Delim` untouched).
ONE block evidence, `Shift.Prompted` (prompt, answer, key, outside row):
`reset`, `delimited`, `scope`, `dollar` all make it, so `exit`, `emit` and
the one-argument `shift`/`shift0` work in any block — their row chosen by
`Shift.RowFor` (the direct block's inside `direct`, the evidence's in a
`for`). `Shift.dynamic` widens a keyed program to mix with captures by
value. Next: one machine guard, `Stacked` as `Shift % p.type`.
specs/shift-merge.md.
