# One offline DLM showcase for the Jev examples

Status: planned, 2026-09-28. Owner lane: `dlm-jev-examples-showcase`.

## Goal

Replace the two small DLM/Jev smoke suites with one executable MUnit test:
`TestDlmJevExamples.showcase`. It demonstrates the business behaviours from
the public Jev examples, not their provider-specific SDK calls.

| Public Jev example shape | DLM showcase section |
|---|---|
| Go quickstart: billing Noul, team Choice, urgency Score for “charged twice … ASAP” | exact billing rule → `billing`, `Billing`, `Today` |
| Ticket triage: route a support message into a team | `Billing`, `Technical`, or explicit `Unclear` |
| Refund request / financial action | route opens review; replay preserves the action; payment ledger authorises or refuses the side effect |
| Typed uncertainty | an unowned text, even with urgency wording, becomes `Unclear` rather than a fabricated score or team |
| Multi-turn support flow | `Pending.Answer` keeps a scoped confirmation from becoming a new command |

## Behaviour

- [ ] There is one suite, `TestDlmJevExamples`, and one test,
      `showcase`, rather than separate quickstart and refund smoke suites.
- [ ] Each section calls `clue` before assertions so a failure names its
      public-example behaviour.
- [ ] The quickstart ticket yields billing, Billing and Today from exact
      authored evidence.
- [ ] A technical ticket yields Technical and ThisWeek; an unowned ticket is
      Unclear with no urgency result.
- [ ] The Russian duplicate-charge dialogue records/replays its action and
      reads the scoped confirmation as pending.
- [ ] A claim for an order with no payment yields `NoPayment` and zero refund
      calls.
- [ ] No network, key, model, Python runtime, remote probability, or SDK is
      needed to run the showcase.
- [ ] The two predecessor test files are removed and the guide points at the
      one command that runs all examples.

## Design

The showcase owns small, explicit domain policy (`Billing`, `Technical`, and
three urgency labels) while calling the library's existing `Router`,
`Decision`, and `Record`. No generic score abstraction is added: Jev's
fractional `Score` and calibrated `Noul` are model outputs, whereas this
fixture deliberately proves a deterministic alternative with explicit rules.

## Verification

```text
scripts/gate.sh "okayDlm/testOnly okay.dlm.TestDlmJevExamples"
```

## Results

Pending implementation.
