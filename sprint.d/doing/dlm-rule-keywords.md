- [ ] dlm-rule-keywords — author an intent's rules as plain keywords
      instead of regular expressions (operator, 2026-09-29).
      `Rule.keywords("payout*", "invoice*", "charged twice")` compiles to
      the canonical `(?iU)\b(?:…)\b` rules `Fuzzy.literalTriggers`
      already mines, so the typo layer covers keywords for free;
      `"keywords"` in intents JSON appends the same rules. Spec:
      specs/dlm-rule-keywords.md. Showcase TestDlmJevExamples rewritten
      on keywords.
