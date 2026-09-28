- [ ] dlm-menu — `Intents.menu(lang)`: the capability menu SELECTED by the
      library (operator, 2026-09-28, asked where it belongs and answered
      "in okay"). The data is already authored — `help` per language,
      `rank`, `internal` as a reason not to offer — and every consumer
      that answers "what can you do?" or offers a choice after an
      `Unclear` re-derives the same three lines: drop the internal ones,
      order by rank then name, take the help cell of the language asked.
      That is selection and order, no words: the sentences stay the
      caller's `Intent.Help`. Also `menu`'s holes — an intent offerable
      in one language and not another is a gap a caller should be able
      to see. specs/dlm.md's Intents row; a test per line.
