## master-warnings-telegram-dlm - master's new okay-telegram and okay-dlm code compiles without warnings; TestChats reads no order it was not promised

- okay-telegram and okay-dlm landed on master with compile warnings that
  a cold compile shows ("no warnings, ever"): a discarded ledger entry in
  `Governed.refuse` (E176), an unused `import okay.given` in `Chats` and
  in `Bot` (E198), an unused private `Update.kinds`, and in TestBot an
  unused `Json.*` import and a discarded send. The send's answer is
  asserted now (`Right(77L)`, the fake's one id) rather than dropped;
  the entry is ascribed `: Unit` with the reason; the rest is deleted.
- TestChats "THE GATE" waited for the edit and then asserted the callback
  answer had already been made. The press is an event for the
  application — its re-render, on its own fiber, is the edit — and an
  `Answer` act the door performs: two independent calls with no order
  between them. Under the `adaptive` default the edit sometimes came
  first; the law now waits for both.
- Cold compile of okayTelegram (JVM, JS) and okayDlm: no warnings;
  okayTelegramJVM 13/13 and TestChats 4/4 twice more, okayTelegramJS
  2/2, okayDlm 106/106.
