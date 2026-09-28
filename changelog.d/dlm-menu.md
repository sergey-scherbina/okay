## dlm-menu - the capability menu selected by the library, worded by the caller

The operator, 2026-09-28, asked where the assembly belongs and answered
"in okay": selection and order are mechanism, the sentences are not.

- `Intents.menu(lang)`: the intents that may be offered — `internal`
  out — ordered by `rank` then name, each with the `Intent.Help` cell of
  the language asked; an intent never described in that language is not
  in the menu rather than in it wordlessly.
- `Intents.menuHoles(languages)`: per language, the offerable intents
  that carry no help cell — the gap `menu` alone cannot show, as
  `Phrasing.holes` is for the phrasings.
- Four tests; specs/dlm.md's Intents row and a behaviour line.
