# Jev quickstart, reproduced locally by DLM

The [Jev quickstart](https://github.com/kataras/jev/blob/main/examples/quickstart/main.go)
sends one ticket — “I was charged twice. Please fix this ASAP.” — and asks
three typed questions: billing, team, and urgency.

The DLM counterpart keeps the business shape but changes the source of truth.

| Question | Jev | Local DLM fixture |
|---|---|---|
| Is this billing? | A `Noul` probability | `true`, because the `charged twice` rule fired |
| Which team? | A choice and probabilities | `Billing`, with the exact matching rule recorded as evidence |
| How urgent? | A potentially fractional score | `Today`, because the authored `ASAP` rule fired |

The same fixture routes “The app is crashing; this week is fine” to
`Technical` and `ThisWeek`. A text with no owned rule, including an unrelated
message that says `ASAP`, stays `Unclear`: it is not silently assigned a team
or an urgency.

This is an intentional difference, not an emulation of Jev. DLM does not
manufacture confidence or a fractional score; it shows the rule and can replay
the result. Neither result proves that a payment occurred — a caller must
continue to consult its payment ledger before any refund.
