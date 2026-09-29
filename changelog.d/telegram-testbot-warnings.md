## telegram-testbot-warnings - the two E176 warnings that kept the runner red

- TestBot.scala:80 and :102 discarded the value `serve` answers;
  `val _ = run(…)`. Every whole-build gate since telegram-live's landing
  was RED on warnings, so the runner pushed nothing (25 commits behind
  origin by 15:00, 2026-09-29).
