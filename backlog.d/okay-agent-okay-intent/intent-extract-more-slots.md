- [ ] intent-extract-more-slots — people DONE 2026-09-07
      (intent-extract-people): `People.parse`/`find` — a count beside a
      people-word (`four people`, `six of us`, `a team of 5`, `vier
      Personen`, `dla czterech osób`, `4人用`), the Slavic collective
      numerals counting by themselves (`на четверых`, `на чотирьох`),
      1..1000, `None` otherwise; `Slots.people`; the fixture's
      book-room row counts four in all eight languages; number words
      shared with `Duration` through `Numbers`. Left here: named
      entities (who) and places — neither is a parser. Durations DONE
      2026-09-07
      (intent-extract-duration): `Duration.parse`/`find` (minutes; a
      number and a unit, `1h30`, `90m`, the spoken fractions, number
      words, `and a half`; total and deterministic like `Temporal`)
      and `Slots.duration`, asked in `when`'s six languages, showing
      `1h30`/`2h`/`45min` back. English phrases; the other languages'
      number-and-unit words are filed as
      intent-duration-multilingual. Amounts DONE 2026-09-07
      (intent-extract-amount): `Amount.parse`/`find` — a number
      beside a symbol, a code or an unambiguous currency name in
      the eight languages, separators told apart by the digits that
      follow, composed number words, the nearest number wins;
      `Amount(value, currency)` with an ISO code; `Slots.amount`.
      Still open here: named entities (who) and places — neither a
      parser. Original: only
      `when` and whole-message text have extractors. Named entities
      (who), durations, places and amounts are the obvious next ones,
      and each is a `Slot.extract` rather than a design.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
