- **refs-typed-heap — REFUTED 2026-09-11.** Scala 3's generalized
  method syntax (a type clause after a term clause) does NOT remove
  `Refs.handle`'s cast, though its own documented example
  (`def getOrElse(k: Key)[V >: k.Value]`) is that signature to the
  letter. Two roads compiled and refused: a `Ref` with `type Value`
  plus `Map[Ref, Slot]` — `s.value` is `s.ref.Value` and nothing proves
  `s.ref` is `c`; and the heap as a dependent function
  `(c: Ref) => Option[c.Value]`, which expresses the dependence and
  then breaks at `put`, where `k eq c` is a run-time fact the types
  never learn. The rule both share: the feature fixes SIGNATURES, not
  STORAGE — it lets a type depend on a term that is present, and does
  not recover a type erasure has thrown away. Written out beside the
  cast in `Refs.scala`; the probes are in the session scratchpad.
  Where the feature WOULD earn its place is a value with a type MEMBER
  whose dependent type cannot be written today; okay has no such API
  at present (`Ref[S]`, `Fact[V]`, `Tag[K,F,A]` all carry parameters,
  not members).

One line each; the measurement and the reasoning are in
`BACKLOG-ARCHIVE.md` under the same slug.
