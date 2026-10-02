## dlm-origin - a table named by what it was built from, not only by its numbers

specs/dlm-learning.md §11, at the operator's word («Окей начинай»),
after Okay!Chat measured the same corpus through the same encoder
giving a new `Exemplars.hash` on every processor.

- `Embedder.fingerprint`: which numbers an encoder's name stands for —
  a model's tensors, an algorithm and its parameters; "" when unknown.
  `hashing` carries one; `Embedder.of` takes one.
- `Exemplars.Provenance(corpus, fingerprint)` and `Exemplars.origin`:
  the digest of exactly the `(label, phrase)` rows embedded, with the
  encoder's name and fingerprint — the same on every CPU. `hash` stays
  what a pin serves. Provenance travels through the checkpoint's
  metadata, the JSON artifact and `stored`; an older table reads with
  none.
- `Exemplars.agrees(a, b, min = 0.9999)`: the same table by meaning, the
  lowest cosine or the first row that disagrees — a build gate's
  question. `Exemplars.accept(table, embedder)`: refused by name, and
  by fingerprint when both sides carry one.
- `Ledger.Entry.Rebuilt` and `Kept` carry the origin; an older ledger
  line reads with none.
- Six tests, one per behavior line; 128 in the module.
