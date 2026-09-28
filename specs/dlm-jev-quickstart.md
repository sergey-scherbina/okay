# DLM counterpart of the Jev quickstart

Status: planned, 2026-09-28. Owner lane: `dlm-jev-quickstart`.

## Source scenario

This reproduces the shape of
[kataras/jev quickstart](https://github.com/kataras/jev/blob/main/examples/quickstart/main.go),
locally:

```text
I was charged twice. Please fix this ASAP.
```

Its Jev request asks three questions in one System One call:

| Jev question | Jev result shape | DLM counterpart |
|---|---|---|
| Is this about billing? | `Noul: Double` | `billing: Boolean`, derived from an authored billing route |
| Which team should handle this? | `Choice` with probabilities | `team: Billing | Technical | Unclear`, with the matching DLM support |
| How urgent is this? | potentially fractional `Score` | `urgency: CanWait | ThisWeek | Today`, from an authored, ordered rule table |

The semantic purpose is held, but the result is intentionally not an emulated
Jev response. DLM has no invented probability or fractional urgency. It says
which rule decided and returns `Unclear` when no authored route applies.

## Behaviour

- [x] A self-contained DLM smoke fixture reads the exact English ticket.
- [x] The ticket routes to the billing team by an exact duplicate-charge rule;
      `billing` follows from that route and carries the same support.
- [x] `ASAP` routes urgency to `Today` through an explicit urgency rule.
- [x] A technical ticket routes to `Technical`; a ticket without any owned
      rule returns `Unclear`, rather than selecting a team or urgency.
- [x] The fixture never calls Jev, Laya, a remote embedding service, or a
      payment provider.
- [x] A guide puts the two contracts side by side and preserves the payment
      authority boundary: neither a Jev result nor a DLM route proves money
      moved.

## Design

This is caller data plus the existing `Router`, not a new generic scoring
framework. An urgency scale is domain policy, so the test owns its three
ordered labels and exact patterns. That keeps the DLM library's promise that
it owns mechanism while the consumer owns words and product meaning.

The direct-API version later may expose the same result over SystemOne through
the existing `SystemOne.Service`, but it must be a separate compatibility
slice. The first proof is in-process and deterministic.

## Verification

```text
scripts/gate.sh "okayDlm/testOnly okay.dlm.TestDlmJevQuickstart"
```

## Results

Implemented in `TestDlmJevQuickstart` and
`docs/guides/dlm-jev-quickstart.md`.

Verified 2026-09-28: `scripts/gate.sh "okayDlm/testOnly
okay.dlm.TestDlmJevQuickstart"` — 2 passed, 0 failed.
