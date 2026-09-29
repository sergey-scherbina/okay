- [ ] telegram-testbot-warnings — TestBot.scala:80 and :102 discard the
      value `serve` answers (E176), which since telegram-live's landing
      has made every whole-build gate RED on warnings and stopped the
      runner from pushing (25 commits behind origin at 2026-09-29 15:00).
      `val _ = run(…)`, the repo's own idiom for a discarded value. A
      test-only change: `okayTelegramJVM/testOnly okay.telegram.TestBot`.
      (2026-09-29)
