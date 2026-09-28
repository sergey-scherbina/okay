# DLM smoke: a refund request is not a refund

This fixture shows the boundary a safe dialogue system needs.

| Turn | Deterministic evidence | Result |
|---|---|---|
| «С меня дважды списали за заказ 4411» | Authored `duplicate-charge` rule; order `4411` | Open a refund review |
| «Да, но только за последний заказ» | `Pending.Answer(refund-confirmation)` | Answer that outstanding confirmation; do not route a new command |
| «С меня дважды списали за заказ 9931» | The same rule; order `9931` | Ask the payment ledger, not the language model |

The dialogue model is deterministic over its state and authored rules. It can
therefore explain why it opened a review and can replay that decision from its
record. It cannot establish that money moved: only the caller's payment ledger
can do that.

The smoke test models this boundary explicitly. A ledger with no payment for
order `9931` produces `NoPayment(9931)` and records no refund side effect.
Text is evidence of what a person requested, never evidence of a settled
payment.

Laya can later be connected to the same scenario only as a typed-judgement
comparison. It is not needed for this proof, and it must not replace the
ledger check.
