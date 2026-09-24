# foreign-workflow: every language's workers inside okay's durable layers

## Overview

The operator (2026-09-24), after stages 5 and 6 of
specs/polyglot-one-wire.md landed: "интегрировать все это в дюрабле и
воркфлоу ... и в статические проц и ду нотейшен". A foreign worker is
now a complete peer on the wire. It has every language, every link
(pipes, TCP, FFM, wasm), the wire's givens (format, compression, auth,
TLS, deadline) and a supervisor that replays programs as data after a
far-side crash. What it lacks is a place in the layers that make okay
programs DURABLE:

| layer | what it is | where a foreign call stands today |
|---|---|---|
| `Durable` (okay-agent) | a journal over any handler: replay without the effect | works: `TestPyJournal`, `TestHaskellProgram` journal `ForeignEval` walks |
| `Wf` workflows (okay-workflow, okay-persist) | questions answered by an oracle, the answers journalled in a topic, written in do-notation (`direct:` with `!w.pause`) | no road: a workflow's author writes the oracle by hand |
| static `Proc` (specs/static-workflow.md) | the same workflow as an arrow term whose leaves are known before it runs | no road |
| proc-notation (specs/proc-notation.md) | `direct` at an arrow | no road |

The one fact the design rests on: a workflow ACTIVITY is a question and
its remembered answer (`Wf.perform` says so in its comment), and a
foreign call is exactly a command performed outside with a result worth
remembering. So there is no second mechanism to build. A foreign call
becomes a question type, the worker becomes the oracle, and every
property of the durable layers applies unchanged: the journal, replay,
crash-resume, versioning (`program` stamps), races (`expect`), timers,
signals and `patch`.

## Interface

A new JVM module, `okay-foreign-workflow` (package `okay.py.workflow`),
depending on okay-py, okay-workflow and okay-persist. okay-py stays free
of the workflow machinery.

```scala
/** a foreign call as a workflow question: its address, its arguments */
final case class ForeignCall(address: String, args: Vector[PyValue])

object ForeignActivity:
  /** the oracle: each question a `start` on whatever ForeignEval handler
   * is installed (a worker, a supervised worker, a pool), answered in the
   * wire's written form of Either[Condition, PyValue]; a transport failure
   * is retried `attempts` times, then thrown as `Unreachable` */
  def oracle: ForeignCall => String ! ForeignEval
  def oracle(attempts: Int): ForeignCall => String ! ForeignEval
  /** a journalled answer, back as a value or a condition */
  def answer(written: String): Either[Condition, PyValue]
  /** a TYPED activity inside a workflow, decoded by Out's Schema:
   * `!ForeignActivity.call[Double]("shop:price")(sku)` */
  def call[Out: Schema](address: String): Call[Out]   // apply(args...)(using Wf.Asks[ForeignCall, String, R, F], At)

/** the same activity as a static Proc leaf, named by its address (stage 2) */
object ForeignProc:
  def call[X: ToPy, Out: Schema](address: String): Wf.Proc[ForeignCall, String, X, Either[Condition, Out]]
```

## Behavior

### Stage 1: activities in a durable workflow (do-notation)

- [x] A workflow written in `direct:` do-notation calls a Python function
      and a Go function as activities. `Dialogue.workflow(...).runWorkflow`
      drives it with `ForeignActivity.oracle` under a worker's handler,
      and the answers land in the topic.
- [x] CRASH-RESUME: the host stops after the first activity was answered
      (a fresh `Dialogue.workflow` on the same topic, as a restarted
      process would open it). The resumed run does NOT call the first
      function again (the far side counts its calls), and it finishes
      with the same answer.
- [x] The FUNCTION's failure (its own exception) is a JOURNALLED
      `Left(Condition)`. The workflow branches on it, and a replay reaches
      the same branch without calling the far side.
- [x] A WIRE failure (the worker died, a deadline, no connection) is
      NOT an answer. It is retried, `attempts` times, on a fresh worker
      when the handler is a supervisor, and then the oracle throws
      `Unreachable`, leaving the step unanswered, so the next run of the
      workflow does it. (Amended while building: see Decisions.)
- [x] The typed call decodes through `Schema`: a far answer of the wrong
      shape is a `Left`, not a crash in the workflow.
- [x] Works with the stage 5/6 givens: the oracle's worker may be
      supervised, compressed, authenticated or encrypted. The workflow
      neither knows nor cares, which is the point.

### Stage 2: the static `Proc`

- [x] `ForeignProc.call(address)` is a Proc leaf. `leaves` names it by
      its address, `mermaid` draws it, and `toProgram` runs it through
      the same oracle (one mechanism: the Proc bridge).
- [x] `walk` over a journal written by the do-notation form of the same
      workflow agrees with it: the static and the monadic form read one
      journal.
- [x] proc-notation: the same workflow as an arrow block
      (specs/proc-notation.md), compiled to the Proc.

### Stage 3: a durable foreign PROGRAM surviving a host crash

`SupervisedWorker` survives a FAR-side crash by replaying paths that live
in the host's memory. A HOST crash loses those paths, and the far
side's continuations with them. `Durable` has the answers in its
journal but never shows them to the supervisor.

- [ ] `Durable.over(supervised.handler, journal)(replayed = supervised.witness)`,
      resumed by a fresh host on the journal of a half-walked multi-shot
      program, finishes every branch. `Durable.over` gains one optional
      parameter, `replayed`, called with each operation it answers FROM
      THE JOURNAL and that answer (a no-op by default, so nothing else
      changes). `SupervisedWorker.witness` rebuilds its continuation
      table from the replayed `Program`/`Continue` records: each
      continuation is marked as belonging to no live worker, so the
      first live `Continue` re-derives it on the fresh far side by the
      stage-6 replay. The two replays compose: Durable's answers from
      the journal, then the supervisor's re-derivation on the far side.
- [ ] Without the witness the resumed host is refused by name
      ("continuation k is not held"), never answered wrongly.
- [ ] A run's id must be the same in the resumed host (it is in the
      fingerprint Durable checks). A durable program keeps its `PyRun`
      id with the rest of its state, and the docs say so.

## Out of scope

- Workflows written IN another language (a Python workflow body with its
  own durable journal). The far side stays a pure function or a program
  as data; durability is okay's.
- Exactly-once side effects in the far side. The durable layers promise
  at-least-once for an activity (durable-workflow.md's case C), and a
  foreign call inherits that promise, stated in the docs.

## Decisions

- **A foreign call is a question, not a new node kind.** `Wf.perform`'s
  comment already says an activity IS a question. A second mechanism
  would need its own journal format, versioning and replay, and it would
  drift from the first.
- **The function's failure is journalled; the wire's is not.** The first
  draft journalled every `Left`, including `WorkerDied`. The Go test
  showed what that means: a server killed between two activities made
  "no total: WorkerDied" the workflow's permanent answer, because a
  network blip was recorded as history. Workflow engines split the two
  the same way, with an application error recorded and an activity retry
  for infrastructure. So an exception the far function raised is an
  answer and is journalled, while a transport condition (`WorkerDied`,
  `timeout`, `WorkerUnavailable`, `WireError`) is retried and then
  thrown as `Unreachable`, unjournalled. That is at-least-once, the
  promise the durable layers already make for an activity (C in
  durable-workflow.md).
- **The oracle uses `start`, not `call`.** Go and Rust serve their
  functions direct-style, so `start` reaches every language. An
  activity offers no callbacks, so a far-side `okay_call` is answered
  with a `NoCallback` refusal: a frame left waiting would hang the
  worker.
- **The journalled answer is the wire's own written form**
  (`Wire.written`), the text `Durable` already journals a foreign call
  in. A workflow's answer type is `String`, which needs no new Schema,
  and `None`, `NaN` and a condition stay distinct.

## Results

- Stage 1 (foreign-in-durable-workflow, 2026-09-24).
  - Module `okay-foreign-workflow`: `ForeignCall`, `ForeignActivity.oracle`
    (`attempts`, `Unreachable`, `transport`), `ForeignActivity.call[Out]`
    (typed, through Schema), and `ForeignActivity.answer`.
  - Live tests. Python: a do-notation workflow; a host crash between
    two activities, with the resumed run leaving the first one's
    far-side count unchanged; a journalled `KeyError` replayed without a
    call; a wrong-shaped answer as a `Left`. Go, over a SUPERVISED,
    CBOR, HMAC-authenticated TCP connection: the server killed and
    restarted between two activities, with the new server doing only
    the second; a server down through every attempt, then the next run
    finishing.

- Stage 2 (2026-09-24).
  - `ForeignProc.call`/`call2`, leaves named by address, going through
    `Wf.Proc.asking`, so there is one mechanism with the monadic form.
    `ForeignProc.ask` and `decode` serve proc-notation, where the helper's
    name names the leaf.
  - ONE TOPIC, THREE FRONT ENDS: do-notation (`Shop.order`), the term and
    the `Proc.direct` block write the same journal, record for record,
    against a live Python worker. `walk` over the do-notation run's
    journal stands at `Done("total 12.0")` without calling anything.
  - Found: a `case` binder is not in proc-notation's environment (backlog
    `proc-notation-case-binders`); the block branches with an `if` over a
    `val`.

