## dlm-laya-smoke — a deterministic refund-request trace with a payment boundary

`okay-dlm` now carries a compact, product-shaped smoke fixture for a Russian
duplicate-charge conversation. An authored rule extracts the order, the
decision records and recalls the resulting action, and a richer follow-up is
read under its explicit pending-confirmation state rather than routed as a
new operation.

The fixture makes the safety boundary executable: a request never proves a
payment. The caller-side `PaymentLedger` authorises a refund only after it
finds settled payments; a forged claim for order 9931 produces `NoPayment`
and starts zero refunds. The guide states the same boundary for the later
Laya comparison: Laya may judge typed text, but it never replaces the ledger.

Verified with `okayDlm/testOnly okay.dlm.TestDlmLayaSmoke` (2 passed) and
`affected master Test/compile` (including DLM and documentation compilation).
Landed as 0cd887c97 and 4b43541ab.
