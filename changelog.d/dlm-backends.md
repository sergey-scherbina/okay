## dlm-backends - okay-dlm: every model backend a seam, ours by default, Jev and Laya by a given

At the operator's word («абстрактное ядро как набор интерфейсов и
подключаемые реализации и дефолтная конфигурация с перегружаемыми
через имплиситы альтернативами»), the three functions a deterministic
model asks of a "model" are traits with our implementation as the
companion's `given` (specs/dlm.md, stage 3):

- `Embedder` — ours: hashed trigrams; `of` for a model on disk,
  `static` for the distilled table. `Exemplars.compile(rows)` through
  the one in scope.
- `Judge` with `Question`/`Choice`/`Fit` — ours: the probe over the
  exemplars, restricted to the options asked; `orElse`, `guarded`
  (timeout, strikes, cooldown) around one that leaves the process.
  `Head.of`, `Router.of`/`judged`, `Dlm.of` summon `Judge.Fit`.
- `Language.Detector` — ours: `Trigrams`; `Judged` asks a judge.
- `Dlm.of` refuses by name a table compiled by an encoder other than
  the one in scope.
- okay-dlm-remote: `Wire` (ours: java.net.http; `Canned`), `SystemOne`
  (the codec and client of the `POST /v1/systemone` wire), `Jev`,
  `Laya`, `Embeddings.openAi`. One protocol, two configurations, every
  test over a canned wire. Measured against nothing here, on purpose.
- The consumer's constructors kept; 89 + 8 tests.
