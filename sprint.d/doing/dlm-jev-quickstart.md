- [ ] dlm-jev-quickstart — reproduce kataras/jev's System One quickstart
      locally in `okay-dlm`: the one support ticket “charged twice” produces
      deterministic `billing`, `team` and `urgency` decisions, without a
      key, network or probability claim. Write `specs/dlm-jev-quickstart.md`
      first; use authored rules/state and explicit deterministic scales; keep
      the payment ledger outside the classifier as in `dlm-laya-smoke`.
      Document the direct correspondence and the deliberate differences:
      Jev returns a Noul probability and possibly fractional score, while
      DLM returns explainable rules/actions and refuses an unmodelled case.
      Done when one self-contained test asserts all three answers and their
      evidence, and the guide can be read beside the Go source.
