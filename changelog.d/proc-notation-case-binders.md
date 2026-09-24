## proc-notation-case-binders - match in proc-notation

Found by foreign-in-durable-workflow: branching on an activity's
`Either` with `match` was refused, and the workflow had to write an `if`
over a `val`.

- `Proc.direct` compiles a `match` whose cases ask questions: a pure
  selector running the original patterns and guards, each case's binders
  joining its branch's environment, the branches joined by `|||`. It
  works in every position an `if` has, and as an assignment's value.
- Found and fixed beside it:
  - an `if` with a question as a statement of its own crashed the macro
    (never exercised before);
  - an assignment to an outer name inside a branch compiled to
    "Reassignment to val _2" and is now refused by name;
  - `x = if …` and `x = … match …` with questions now compile.
- Refused by name: a question in a guard, a `match` with questions
  nested in a larger expression.
- Tests: TestProcMatch (12). The okay-foreign-workflow block and the
  one-language.md example now use `match`. Docs: static-workflows.md.
