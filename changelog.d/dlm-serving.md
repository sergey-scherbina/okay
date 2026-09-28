## dlm-serving - our model on the System One wire; the learning mode specified

- okay-dlm-remote `SystemOne.Service`: `serve(body, judges)` answers a
  Jev/Laya-shaped `POST /v1/systemone` from our own judges. A question
  reaches the head whose classes it names; `noul` is a yes/no choice;
  `score` an expectation over the levels; a question nobody ranks is an
  `error` in its own slot while the rest are answered; no `confidence`
  is sent that a judge did not have. Round-trip tested through our own
  client. A client written against either vendor runs against this
  model unchanged.
- specs/dlm-learning.md: the learning mode governed, audited,
  explainable and correctable — `Teaching` (rights, ours the
  narrowest), `Ledger` (append-only, refusals included), `Explanation`
  (a value, not a sentence), `Governed` (the doors inside and out),
  with the invariant no channel may cross: learning never creates a
  class, edits a rule or moves a threshold. Spec only; the code is
  stage 5.
