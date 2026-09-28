## dlm-shelf - every table kept by hash, dropped only by a policy (dlm-learning stage 3)

specs/dlm-learning.md §9, at the operator's word: «старые чекпойнты
храним, но можем их потом удалять согласно политике».

- `Shelf` / `Kept`: every table a model has served, kept by the hash of
  the table AS SERVED (`Exemplars.stored`, F16 rounding included) — the
  hash an explanation names, the hash in `Rebuilt` and the file name are
  one string. Ours: `directory`, `memory`, and `resource` for a pin
  served from an image. `get` refuses another encoder by name and a
  file whose content no longer hashes to its name.
- `Retention`: which old tables may go — ours `all` (none), `latest`,
  `within`, `any`, `of("last:5,days:30")`. `Shelf.prune` spares the
  newest table of each artifact and every serving one, whatever the
  policy says, and returns a `Pruned` entry naming the policy.
- `Ledger`: `Pruned`; `Ledger.File` keeps entries as JSON lines, and
  `parse` skips a kind it does not know.
- `Checkpoint.meta`: a header without the vectors.
- `Judge`: the two `Embedder`-as-function uses are explicit, so a cold
  compile under `-feature` is clean.
- Five tests, one per library behavior line of §9.
