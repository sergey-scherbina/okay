## growing-order-entries-closed - the two growing-order entries say the decision was taken

`growing-order-drain-guarantee` and `growing-channel-order-under-load`
(backlog.d/okay-core) were written on 2026-09-18 before the operator
closed the per-producer-order question that afternoon as a documented
trade (8af62bc7: the default channel keeps a producer's order except
once, across its one-shot swap; `adaptive` and `fifo` keep it exactly;
the law is in `TestChannelLaws`, the table in docs/queues.md, the
choice in `ActorRef`'s header). Neither entry mentioned the closure,
and the first still argued against the weakening — so a reader took
them for an open defect in the default channel, twice in one review
(2026-09-20). The first is now the optional buy-back it was declared
to be, with its price (seal part 0 at adoption; three lanes) and the
two refuted candidates kept; the second moves to
refuted-declined-or-answered as a one-paragraph verdict pointing at
BUGS.md, which carries the ledger.

Files: backlog.d/okay-core/growing-order-drain-guarantee.md,
backlog.d/refuted-declined-or-answered/growing-channel-order-under-load.md
(moved from okay-core).
