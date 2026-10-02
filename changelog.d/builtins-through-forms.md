## builtins-through-forms - Reader and State through the author's forms

Level 2 (operator's roadmap, 2026-10-02). `Reader(r)` is
`Handler.answerOf` and `State(s)` is `Handler.stateOf`. `Reader.run` and
`State.handle` delegate to them, so each effect has one implementation and
the built-ins are worked examples of the forms. Measured: Reader 1.04x,
within its noise, the same bytes. State's loop through the form is 1.04x of
a hand loop over the same two operations. Kept by the operator's call
(optimise later, backlog `state-write-cost`).
