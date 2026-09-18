- [x] route-secured-with-a-value — DONE 2026-09-11, in two steps the
      same day. `media`/`html` came first, because okay-demo's document
      rendered `/admin/replay` with a 401, a 403 and NO SUCCESS CASE
      and the demo's own guard caught it; the rest — `htmlAt`,
      `bytes`, `bytesAt`, `events`, `eventsAt`, `json`, `jsonAt`,
      `out`, `outAt`, `jsonOut`, `jsonOutAt`, on the class and the
      companion — came when the operator asked. Each carries
      `securityAnswers` beside whatever it declares itself.
      The wrinkle a caller meets: the `Headed` forms carry NO default
      arguments (Scala allows them on one overload of a name), and
      `status`/`description` must be passed POSITIONALLY, because a
      named argument narrows overload resolution before the argument
      types are read.
