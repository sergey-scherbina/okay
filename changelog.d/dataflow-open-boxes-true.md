## dataflow-open-boxes-true - what is left of dataflow is blocked or decided, and two boxes did not say so

After `dataflow-horizon-seek`, `dataflow-windowed-rescale`,
`dataflow-kafka-eos`, `federation-refusal` and `dataflow-cas-commit`,
the unticked boxes in specs/dataflow.md were audited one at a time
against the code. What they are:

- BLOCKED ON MACHINES, and not to be pretended at: stage 12's three
  (the cross-process harness over a real link, injected latency and
  loss, a partition between coordinator and worker) and stage 7's
  real cluster. The operator's constraint is one machine; these need
  more than one and the spec says so.
- DECLINED IN THEIR OWN TEXT: the optimizer pushing a combine below
  an exchange ("STILL NOT APPLICABLE" — the combine is where a keyed
  stage BEGINS here, so no plan reaches the exchange uncombined), and
  a batch `Cluster.run` that resumes ("a two-pass function with
  nothing to resume from, and restarting it is the answer").
- A TRIGGER, not a task: a writer whose stage is DURABLE. The seam
  carries one; nothing has asked.

TWO READ AS UNBUILT WORK AND WERE NOT. The batch-resume box argued
its own refusal and still sat unticked, so it is `[~]` now with the
decision named. And the coordinator ELECTION box — "okay-persist has
`Election`; nothing here asks for it yet" — was reading two halves as
one: the engine's half landed in STAGE 10 (`Cluster.leading` takes a
`Lease`, `TestPersisted`'s `Elected` binds the real `Election` to it,
and the two-nodes-one-seat test is ticked there), while the half that
is genuinely absent is a loop that WAITS to be elected — which
`Cluster.leading`'s own Scaladoc declines, with the composable
snippet, because a retry loop needs a clock and a backoff that belong
to whatever supervises the process.

Nothing was built here. The point of the lane is that the next person
to read the spec should not start work that is finished, and should
not mistake a decision for an omission.
