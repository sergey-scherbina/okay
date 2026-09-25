- [ ] foreign-map-reduce — map-reduce whose MAP runs in Python or R
      (operator, 2026-09-25: "Делай", after "ты можешь сделать map-reduce
      для вычислений на R или Python, или Rust, Haskell, Frege, Clojure").
      okay-cluster's `Job`/`Flow` already IS map-reduce (partitions, a
      chunk-level `Flow.Local`, `Wire` as the reduce), JVM-only; the
      foreign engines exist; nothing joins them. v1: a `Flow.Local` stage
      whose chunk goes through a foreign worker as ONE Arrow frame
      (`ForeignEval.Frame`/`REval.Frame`, the transport py-arrow/r-arrow
      built), rows typed by `Schema`; reduce stays the JVM `Wire`; one
      supervised process per worker JVM; the foreign code ships as an
      inline module inside the job (same artifact everywhere). New JVM
      module okay-foreign-cluster. Then Clojure/Frege (in-JVM, a function),
      then Rust/Haskell/Go once their shims serve `frame`. Spec first:
      specs/foreign-map-reduce.md.
