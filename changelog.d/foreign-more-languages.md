## foreign-more-languages — TypeScript, Go, Rust and Haskell through the facade and the cluster (2026-09-26)

Every wire language now has its module type in okay-foreign-cluster:
`TsModule` (TypeScript source, run by Node) and `WorkerModule` (a compiled
Go, Rust or Haskell worker, named by the command that starts it). Each is
one `Language` value under the one body every capability already had —
Calls, Frames, Streams, Programs, Speaks, the cluster's Engine and Reduces
for both, and Holds, Methods, Stateful and Models for TypeScript, whose
worker keeps held objects (compiled workers wait for foreign-held-values).
The facade conformance body passes over all four, a partitioned cluster
stage included. Docs: okay-foreign-cluster, okay-cluster, foreign-facade.
