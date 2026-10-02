## refine-match - a pattern is an extractor of a plain match

`Refine.unapply`: every pattern is a case of a plain Scala `match` — a
nested pattern is a path (`case trade(swap((ccy, n))) if ccy == "EUR"`),
recognition and routing in one construct; `Unclear` and `Declined` match
no case, so a match never takes one reading of an ambiguous document. In
okay and okay2 (TestRefineMatch in both). Found: a `Dispatch` lane of a
tuple type is unchecked (E092), so a lane's type must be checkable at run
time — documented. Backlog: refine-cases-macro (each case a named refine
step), refine-lanes-as-refine (Routes / Dispatch / Router as one).
