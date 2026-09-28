# Jev examples, one local deterministic DLM showcase

Run one test to see every DLM counterpart:

```text
scripts/gate.sh "okayDlm/testOnly okay.dlm.TestDlmJevExamples"
```

| Jev/SystemOne example | Local deterministic result |
|---|---|
| “I was charged twice. Please fix this ASAP.” | `billing=true`, `Billing`, `Today`, with the exact rule recorded |
| Technical support triage | `Technical`, `ThisWeek` |
| Unknown text | `Unclear`, no fabricated team or urgency |
| Refund-request workflow | authored route → `Action` → replayable record |
| Confirmation | explicit `Pending.Answer`, never a fresh command |
| Financial safety | missing ledger payment → `NoPayment`, zero refund calls |

This reproduces the useful product behaviours from the public Jev examples,
not their Go/Python SDK or hosted probability API. DLM does not return a
calibrated `Noul` or fractional `Score`; it returns an authored rule or an
honest refusal. Neither system's text judgement proves a payment: that fact
continues to come from a payment ledger.
