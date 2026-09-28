# DLM smoke: one dialogue, safe product boundary

Status: planned, 2026-09-28. Owner lane: `dlm-laya-smoke`.

## Goal

Give a reader one reproducible Russian support dialogue that shows a
deterministic **product decision** and its safe boundary. It must show that a
local DLM needs neither a network call nor a generative answer to replay an
authored action exactly.

The demonstration is deliberately narrow:

```text
Client:  С меня дважды списали за заказ 4411. Верните лишнее.
System:  Вернуть последний платёж по заказу 4411?
Client:  Да, но только за последний заказ.
```

The first turn is an authored rule (`duplicate-charge`) with an extracted
order number. The second turn is read in the explicit `Pending.Answer`
state: it is an answer to an outstanding confirmation, not a newly routed
intent. The production caller owns the actual refund tool and wording.

The same route is deliberately **not** proof that money is owed. A caller's
payment ledger is the authority: `duplicate-charge` opens a review, and a
refund can be performed only after the ledger finds the relevant settled
payments. Thus the malicious or mistaken claim “refund order 9931” where the
ledger has no payment returns `NoPayment(order = "9931")` and makes no
refund call. The model classifies the request; it does not invent a financial
fact.

## Behaviour

- [ ] `okay-dlm` has a self-contained smoke suite for the transcript.
- [ ] The first line routes through `Support.Exact` and decides
      `Action.Act`; its record round-trips and recalls the same action.
- [ ] The confirmation line in `Pending.Answer` decides
      `Action.AnswerPending`, even though it contains more than a bare
      yes/no token. The suite records the pending action supplied by the
      caller.
- [ ] The suite asserts the visible evidence: rule, slot, state, action and
      record — no timing or model-quality number is invented.
- [ ] A pure caller-side payment fixture proves that a routed refund claim is
      only a review until ledger evidence authorises it; a nonexistent payment
      yields `NoPayment` and records zero refund operations.
- [ ] Documentation gives the trace and the comparison boundary for a later
      Laya lane: DLM decides an action from explicit state and authored data;
      a future Laya run may only judge the same typed question.

## Design

The deterministic suite does not start Laya, download weights, use Python,
or depend on a key. It is the CI proof. A later comparison can use the
existing `Laya.fromEnv`/SystemOne seam, but does not belong in this slice.

The action is intentionally generic (`duplicate-charge`) rather than a
refund execution. `Decision.Action` says what the product should do; the
caller owns the irreversible operation and checks the payment ledger before
it. This is not an anti-fraud model and it cannot infer a payment from text:
the safe boundary is an explicit, auditable tool result.

No alternate runtime is introduced in this slice. Laya comparison and a
future Python-free ONNX implementation are separate tasks once this fixture
has proved useful.

## Verification

```text
scripts/gate.sh "okayDlm/testOnly okay.dlm.TestDlmLayaSmoke"
```

## Results

Pending implementation.
