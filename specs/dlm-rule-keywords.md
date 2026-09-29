# DLM rules as keywords

Status: implemented, 2026-09-29. Owner lane: `dlm-rule-keywords`.

## Goal

An intent's `rules` are regular expressions today. Most authored rules
are only a list of trigger words: `(?i)\b(?:payouts?|invoices?)\b`.
The author should write the WORDS, and the library writes the regex.

## Behaviour

- [x] `Rule.keywords(words*)` returns the rules for the words given,
      as `Vector[String]`, so it goes straight into `Intent(rules = …)`
      and can be combined with hand-written regexes by `++`.
- [x] A plain word matches a whole word, case-insensitively, in any
      script: `bug` matches “Bug” and not “debugger”; `ошибка` matches
      “Ошибка”.
- [x] A word ending in `*` matches as a word PREFIX: `payout*` matches
      “payout” and “payouts”, `crash*` matches “crashes”.
- [x] A keyword containing spaces is a PHRASE: its words in order,
      separated by any whitespace (`charged twice` matches
      “charged  twice”); the last word may carry `*`.
- [x] Regex metacharacters in a keyword are literal (`c++` matches
      “c++” only).
- [x] Plain words and prefix words compile to the canonical shape
      `(?iU)\b(?:w1|w2)\b` / `(?iU)\b(?:w1|w2)\w*\b`, which
      `Fuzzy.literalTriggers` mines, so a typo of a keyword reaches the
      typo layer exactly like a hand-written trigger rule.
- [x] An empty or blank keyword is refused (IllegalArgumentException
      naming it) — it would match nothing, silently.
- [x] Intents JSON accepts `"keywords": [...]` beside `"rules"`; the
      compiled rules are appended to the regex rules.
- [x] The Jev showcase (`TestDlmJevExamples`) is authored with keywords
      only, and its guide shows that.

## Design

A function that produces rule strings, not a new `Intent` field: the
router, `Support.Exact`, explanations and the typo miner all keep
reading one list of regexes, and nothing downstream changes. A word is
quoted character by character only where it is not a letter or digit,
so a plain word stays in the literal form the typo miner accepts.

## Verification

```text
scripts/gate.sh "okayDlm/testOnly okay.dlm.TestRuleKeywords okay.dlm.TestDlmJevExamples"
```

## Results

Implemented as `okay-dlm` `Rule.keywords` (Rule.scala) and the
`"keywords"` key of `Intents.parse`; covered by `TestRuleKeywords`
(seven tests, one per behaviour above) and the rewritten
`TestDlmJevExamples`. `okay-dlm` now depends on okay-test at test scope,
for the new suite's `Munit.Diagnosed`.

A word with anything but letters and digits (`c++`, `help!`) cannot use
`\b` — there is no word boundary beside a `+` — so it and every phrase
are their own rules bounded by `(?<!\w)`/`(?!\w)`. The typo layer skips
those, as it skips every structural rule; only plain and prefix words
are typo-tolerant.

Slots are still regexes: a slot captures a value, and a keyword names
no value to capture.

Verified 2026-09-29: `scripts/gate.sh "okayDlm/testOnly
okay.dlm.TestRuleKeywords okay.dlm.TestDlmJevExamples okay.dlm.TestIntents
okay.dlm.TestFuzzy; okayDeploy/testOnly okay.deploy.TestDocSnippets"` —
GREEN, 27 tests, no warnings.
