# okay-foreign-workflow

Foreign workers inside okay's durable workflows (specs/foreign-workflow.md).
A workflow ACTIVITY is a question and its remembered answer, so a foreign
call (Python, TypeScript, Haskell, R, Go, Rust, over any link) is a
question, `ForeignCall`, and the worker is the oracle. The journal,
replay, crash-resume, versioning, timers, signals and `patch` of
okay-workflow and okay-persist apply as they are.

| | |
|---|---|
| `ForeignCall` | a foreign call as a workflow question: the function's address and its arguments |
| `ForeignActivity.call[Out](address)(args)` | a typed activity in do-notation, its answer decoded by `Out`'s Schema; a wrong shape is a `Left` |
| `ForeignActivity.oracle` | the worker as the oracle: a `start` on whatever `ForeignEval` handler is installed (a worker, `ForeignWorker.supervised`, a pool) |
| `ForeignActivity.Unreachable` | thrown when the wire failed through every attempt: the step stays UNANSWERED, so the next run does it |

## Using it

```scala
  def order(sku: String)(using w: Wf.Asks[ForeignCall, String, String, Pure]): String ! Delim + Pure = direct:
    val price = !ForeignActivity.call[Double]("shop:price")(sku)
```

```scala
    val run = Dialogue.workflow[ForeignCall, String, String, Pure](topic, "order-1", "order/1")(order("tea"))
      .runWorkflowIn(q => ForeignActivity.oracle(q))
```

- The FUNCTION's failure (its own exception) is a journalled answer, and a
  replay reaches the same branch without a call.
- The WIRE's failure (a dead worker, a deadline, no connection) is retried
  (on a fresh worker under a supervisor) and never journalled.
- At-least-once, as for every activity: make an activity that reaches the
  outside world idempotent.

The whole story, with the tests behind it: [one language](../one-language.md#a-foreign-call-as-a-workflow-activity).
