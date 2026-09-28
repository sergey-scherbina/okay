## dlm-jev-quickstart — Jev's three-question support example, locally and deterministically

The `kataras/jev` quickstart's ticket — “I was charged twice. Please fix this
ASAP.” — now has a DLM counterpart. Its authored rules return the same
business-shaped answers: billing, the `Billing` team, and `Today` urgency;
the matching rule travels as evidence instead of a manufactured probability.

The fixture also sends a technical ticket to `Technical`/`ThisWeek` and leaves
an unowned message as `Unclear`, even when it says `ASAP`. The accompanying
guide states the direct correspondence and the crucial boundary: text cannot
prove a payment, so any refund still needs ledger evidence.

Verified with `okayDlm/testOnly okay.dlm.TestDlmJevQuickstart` (2 passed) and
`affected master Test/compile`. Landed as f34808be8 and f8af86261.
