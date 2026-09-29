## dlm-rule-keywords — DLM rules written as keywords, not regexes

Operator ask (2026-09-29): an intent's rules should not need regular
expressions. `Rule.keywords("payout*", "invoice*", "charged twice")`
(okay-dlm, Rule.scala) answers the rule strings: a plain word matches a
whole word in any case and script, `word*` a word prefix, a keyword with
spaces a phrase over any whitespace, metacharacters literal, a blank
keyword refused by name. Plain and prefix words compile to the canonical
`(?iU)\b(?:…)\b` shapes `Fuzzy.literalTriggers` mines, so keywords are
typo-tolerant for free (“pricng” reaches `sales`). Intents JSON takes a
`"keywords"` array beside `"rules"`. The Jev showcase
(`TestDlmJevExamples`, docs/guides/dlm-jev-examples-showcase.md) is now
authored with keywords only. Spec: specs/dlm-rule-keywords.md.
