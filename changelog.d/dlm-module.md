## dlm-module - okay-dlm: a deterministic dialogue language model, as a library

At the operator's word — the open library holds everything but the
data; the service holds the data — the mechanism Okay!Chat had
hand-rolled for a month is lifted into a module of its own
(specs/dlm.md, stage 1).

- `okay.dlm`: `Intents` (the authored set as data, validated),
  `Router` with `Route`/`Support` (a lesson, a rule, a typo, a
  probability — and `Unclear` first-class), `Head` (one question by
  vectors or silence, one shape for what were five classes),
  `Exemplars` + `Checkpoint` (the compiled table as JSON and as
  safetensors, refused by encoder name), `Memory` (the taught fold with
  its exact and near bands), `Decision` (the pure turn decision, its
  `State`/`Evidence`/`Action`, and the `Record` a replay resumes from),
  `Language` (cues, trigram detector, profiles), `Confirm`, `Phrasing`,
  `Fuzzy`/`Script`/`Alphabet`, `ModelChain`, `Calibration`, and `Dlm`
  as the whole.
- No words in any language, no rules, no phrases: every one is the
  caller's. Languages are codes; the alphabet is a table of exclusive
  letters per language, not four hard-coded sets.
- Found on the way: the source's mixed-script check read ASCII letters
  only (`\W` without `(?U)`) and could never see the case it was
  written for; the library reads `\p{L}+` and holds «наprawiam» down.
- 81 tests over the hashing embedder; nothing needs a model on disk.
  JVM only. Stage 2 — the service switching to it — is filed, not
  started.
