- [ ] dlm-jev-examples-showcase — consolidate the local DLM counterparts of
      the Jev/SystemOne examples into ONE executable showcase test. First
      write `specs/dlm-jev-examples-showcase.md`; replace
      `TestDlmLayaSmoke` and `TestDlmJevQuickstart` with one suite and one
      named test that checks support triage (billing/technical/unowned),
      urgency, refund-request state and replay, and the payment-ledger
      refusal. Preserve each assertion's failure clue. This is a DLM
      reproduction of business behaviours, not a port of provider SDK calls,
      remote probabilities or model-quality claims. Done when one test runs
      offline and its guide maps each section to the public Jev examples.
