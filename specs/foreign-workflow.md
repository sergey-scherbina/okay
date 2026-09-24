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
final case class ForeignCall(address: String, args: Vector[PyValue]) derives Schema

/** what a foreign activity answers, journalled: the value, or the
 * condition (a far-side failure, a timeout, a death) — a failure is a
 * RECORDED answer too, so a replay does not re-run a call that failed */
type ForeignAnswer = Either[Condition, PyValue]

object ForeignActivity:
  /** the oracle: each question a call on whatever ForeignEval handler is
   * installed (a worker, a supervised worker, a pool) */
  def oracle: ForeignCall => ForeignAnswer ! ForeignEval

  /** a TYPED activity inside a workflow: the call is journalled, its
   * answer decoded by Out's Schema */
  def call[Out: Schema](address: String)(args: PyValue*)
                       (using Wf.Asks[ForeignCall, ForeignAnswer, R, F]): Either[Condition, Out] ! Delim + F

/** the same activity as a static Proc leaf, named by its address */
object ForeignProc:
  def call[X: ToPy, Out: Schema](address: String): Wf.Proc[ForeignCall, ForeignAnswer, X, Either[Condition, Out]]
```

## Behavior

### Stage 1: activities in a durable workflow (do-notation)

- [ ] A workflow written in `direct:` do-notation calls a Python function
      and a Go function as activities. `Dialogue.workflow(...).runWorkflow`
      drives it with `ForeignActivity.oracle` under a worker's handler,
      and the answers land in the topic.
- [ ] CRASH-RESUME: the host stops after the first activity was answered
      (a fresh `Dialogue.workflow` on the same topic, as a restarted
      process would open it). The resumed run does NOT call the first
      function again (the far side counts its calls), and it finishes
      with the same answer.
- [ ] A far-side failure (an exception, a timeout) is a JOURNALLED
      `Left(Condition)`. The workflow branches on it, and a replay
      reaches the same branch without calling the far side.
- [ ] The typed call decodes through `Schema`: a far answer of the wrong
      shape is a `Left`, not a crash in the workflow.
- [ ] Works with the stage 5/6 givens: the oracle's worker may be
      supervised, compressed, authenticated or encrypted. The workflow
      neither knows nor cares, which is the point.

### Stage 2: the static `Proc`

- [ ] `ForeignProc.call(address)` is a Proc leaf. `leaves` names it by
      its address, `mermaid` draws it, and `toProgram` runs it through
      the same oracle (one mechanism: the Proc bridge).
- [ ] `walk` over a journal written by the do-notation form of the same
      workflow agrees with it: the static and the monadic form read one
      journal.
- [ ] proc-notation: the same workflow as an arrow block
      (specs/proc-notation.md), compiled to the Proc.

### Stage 3: a durable foreign PROGRAM surviving a host crash

`SupervisedWorker` survives a FAR-side crash by replaying paths that live
in the host's memory. A HOST crash loses those paths, and the far
side's continuations with them. `Durable` has the answers in its
journal but never shows them to the supervisor.

- [ ] `Durable.over(supervised.handler, journal)`, resumed by a fresh
      host on the journal of a half-walked multi-shot program, finishes
      every branch. The replayed `Program`/`Continue` records rebuild the
      supervisor's paths (design in the Decisions, after a probe).

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
- **A failure is journalled.** Recording only successes would make a
  replay call the far side again for every failed step, which is a
  side effect the journal exists to prevent.

## Results
