## foreign-workflow-activities - foreign calls as durable workflow activities

Stage 1 of specs/foreign-workflow.md. The operator asked to bring the
foreign workers into "дюрабле и воркфлоу ... статические проц и ду
нотейшен".

- New module okay-foreign-workflow: `ForeignCall` (a question),
  `ForeignActivity.oracle` (the worker as the oracle, over `start`, so
  every language answers it) and `ForeignActivity.call[Out](address)(args)`
  (a typed activity in do-notation, decoded by Schema).
- The FUNCTION's failure is a journalled answer. The WIRE's failure is
  retried (on a fresh worker under a supervisor) and then thrown as
  `Unreachable`, unjournalled. This was found by the Go test: the first
  draft recorded "WorkerDied" as a workflow's permanent answer.
- Tests, live: Python (do-notation, host crash-resume without a second
  call, a journalled KeyError, a wrong-shaped answer) and Go over a
  supervised, CBOR, authenticated TCP connection (the server killed
  between two activities; unreachable, then finished by the next run).
- Docs: one-language.md, "A foreign call as a workflow activity". Stages
  2 (static Proc, proc-notation) and 3 (a durable foreign program across
  a host crash) remain on the card.
