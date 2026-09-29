# Jev examples, one local deterministic DLM showcase

The DLM counterpart of the Jev SDK's triage example
([scala-jev-sdk](https://github.com/ticofab/scala-jev-sdk),
`examples/Triage.scala`). Run it with:

```text
scripts/gate.sh "okayDlm/testOnly okay.dlm.TestDlmJevExamples"
```

| Message | Result |
|---|---|
| “Help! My payouts have been failing for 3 days and nobody has replied.” | `Some(Team.Billing)`, urgent |
| “The app crashes when I open settings.” | `Some(Team.Technical)` |
| “Is there a discount if we upgrade to the annual plan?” | `Some(Team.Sales)` |
| “What is the weather like?” | `None` — no team is invented |
| “I was charged twice for order 4411.” (ledger holds two payments) | `Refund.Started("4411")` |
| “I was charged twice for order 9931.” (no payment in the ledger) | `Refund.NoPayment("9931")` |
| “I was charged twice!” | `Refund.AskOrder(...)` — the missing slot is asked for |

Jev answers with calibrated probabilities from a hosted model (`noul`,
`choice`, `score`). DLM answers from authored rules, offline, and returns
`None` rather than a guess. Neither proves a payment: that fact comes from
the ledger.
