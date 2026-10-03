## free-effects-comments - Free.scala and Effects.scala comments trimmed to what is true and needed

Operator: "почисть немного комментарии в Free и Effects" (tidy up the
comments in Free and Effects a bit). Comments only: the code is
byte-identical once comments are stripped (compared).

**What changed:**
- **Trimmed:** each comment keeps its reason and the measured fact a
  reader needs. The lane names, dates, refuted first cuts and their
  stories went; the changelog and specs keep those.
- **Two structural fixes in Effects.scala:**
  - an orphan doc where `resume` used to be is gone, along with the
    history of the old `Effect` alias;
  - the "interpret F into another row" doc, stacked on top of
    `interpret`'s own, now sits on `translate`, which it describes.

Free.scala went from 430 to 249 lines; Effects.scala from 777 to 468.
