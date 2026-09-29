# DLM rules as keywords

Status: planned, 2026-09-29. Owner lane: `dlm-rule-keywords`.

## Goal

An intent's `rules` are regular expressions today. Most authored rules
are only a list of trigger words: `(?i)\b(?:payouts?|invoices?)\b`.
The author should write the WORDS, and the library writes the regex.

## Behaviour

- [ ] `Rule.keywords(words*)` returns the rules for the words given,
      as `Vector[String]`, so it goes straight into `Intent(rules = …)`
      and can be combined with hand-written regexes by `++`.
- [ ] A plain word matches a whole word, case-insensitively, in any
      script: `bug` matches “Bug” and not “debugger”; `ошибка` matches
      “Ошибка”.
- [ ] A word ending in `*` matches as a word PREFIX: `payout*` matches
      “payout” and “payouts”, `crash*` matches “crashes”.
- [ ] A keyword containing spaces is a PHRASE: its words in order,
      separated by any whitespace (`charged twice` matches
      “charged  twice”); the last word may carry `*`.
- [ ] Regex metacharacters in a keyword are literal (`c++` matches
      “c++” only).
- [ ] Plain words and prefix words compile to the canonical shape
      `(?iU)\b(?:w1|w2)\b` / `(?iU)\b(?:w1|w2)\w*\b`, which
      `Fuzzy.literalTriggers` mines, so a typo of a keyword reaches the
      typo layer exactly like a hand-written trigger rule.
- [ ] An empty or blank keyword is refused (IllegalArgumentException
      naming it) — it would match nothing, silently.
- [ ] Intents JSON accepts `"keywords": [...]` beside `"rules"`; the
      compiled rules are appended to the regex rules.
- [ ] The Jev showcase (`TestDlmJevExamples`) is authored with keywords
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
