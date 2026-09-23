## scala2-form - okay-ui's forms from Scala 2.13: FormState[A]

After the five areas landed, the operator said "Продолжай", so the
remaining gaps are next. Forms come first because they are the
smallest.

- okay-ui's `Form` renders a form from a `Schema` and folds edits into
  the value. Its functions all speak `okay.codec.Json`, which Scala 2
  cannot read. `okay.scala2.FormState[A]` (okay-scala2-ui) keeps that
  value inside a value class and exposes `view`, `edit(event)`,
  `errors`, `decoded`, `json`, `withLabels`, `blank[A]` and `of(a)`.
  It is an ordinary immutable state that drops into `UiApp.run`.
- `TestFormFromScala2` (5 tests, green on their first run): a blank
  form's missing fields, edits folded into a decodable value, a
  round trip from a value, the form as a `UiApp` loop's state, and
  labels.
- Queued: `scala2-ws`, `scala2-choose-search`, `scala2-dialog-nav`.
- Docs: section 8f of docs/scala2.md (copied from the probe), the
  okay-scala2-ui page, API reference, and spec stage 11.
