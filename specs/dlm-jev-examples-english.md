# DLM Jev examples in English, shaped like the Jev SDK

Status: done, 2026-09-29. Owner lane: `dlm-jev-examples-english`.

## Goal

`TestDlmJevExamples` follows the shape of the Jev SDK's own example
(https://github.com/ticofab/scala-jev-sdk, `examples/Triage.scala`): one
`Triage` object with a typed `Team`, one inbound message, and the answers
read back as values. Every fixture is English; nothing beyond triage and
the refund ledger boundary.

## Behaviour

- [x] No Russian text in the suite, its guide or its spec.
- [x] The Jev message yields `Some(Team.Billing)` and is urgent.
- [x] Technical and sales messages route to their teams; an unowned one is
      `None`.
- [x] A refund for an order the ledger shows paid twice starts; one with no
      payment is `NoPayment` and starts nothing; one without an order number
      asks for it.
- [x] Still one offline test, `showcase`.

## Decisions

Dropped from the predecessor: replay of a recorded action and the pending
confirmation. Both are library behaviour covered by `TestDecision`; in a
Jev-shaped example they were noise.
