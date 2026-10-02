## okay-macros-package-handler - Handler's case-form macros moved into okay.macros

Stage 2 of okay-macros-package (operator: "Может быть перенесем макросы
в пакет okay.macros", i.e. move the macros into the okay.macros package).
It waited on handle-frames-forms, which has landed.

`Handler.Seen.of` and the checks behind the `{ case … }` forms
(`checkAnswers`, `checkStates`, `checkInto`) now splice
`okay.macros.HandlerMacros`, `@publicInBinary private[okay]`, like
stage 1's objects. The moved code is `seenImpl`, `checkImpl`,
`caseDefsOf` and `checkCore`. Handler.scala no longer imports
`scala.quoted`. Three inventory rows (`ctor`, `mentions`, `names`) were
re-filed under the new path. It is a move only: no behaviour change.

Left: stage 3, the satellites (okay-direct's macros are in package
`okay` today).

Tests: TestHandlerForms, TestHandlerFor; every dependent compiled.
