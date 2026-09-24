## okay2-row-doc-fix - docs/okay2.md: the Scala 3 core has a `Row` too, since rowlift-to-row

- The "why `Row`" paragraph (okay2-row-name-doc, the same day) said the
  Scala 3 core "has no `Row` type at all"; rowlift-to-row renamed
  `okay.RowLift` to `okay.Row` an hour later, which made that false. It
  now says what each `Row` is: in Scala 3 the object of row membership
  (`Row.at`, `Row.In`, `Row.Sub`, `Row.Has`), in okay2 the row's type.
