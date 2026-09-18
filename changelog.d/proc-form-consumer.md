## proc-form-consumer - a form as a durable procedure, and the one thing it could not say

`static-workflow` proved the machinery: an optic applies to a step with
no new code, the journal holds the step's answers and nothing else, and
a run resumes where it stopped. It could not prove that the TERM READS
WELL when the questions are a form's FIELDS rather than a workflow's
activities — the only kind of question a consumer answers. So a signup
form was written as a `Proc`, resumed from its journal, and rendered by
okay-ui.

WHAT WORKS, with no new machinery: a half-filled form IS a journal —
`Wf.advance` stands at the next field holding the answers so far, and
`Wf.replay` over the same journal comes back to the same field with
the earlier answers not asked again. The position is a PATH, so a page
can draw where the user stopped without replaying anything. And a
standing question renders as an ordinary one-field `Ui.Form` whose
`Ui.submit` IS the answer — nothing on the UI side is special to
`Proc`, which is the point: a form is a form.

WHAT IT COULD NOT SAY, and the reason `Wf.Proc.asking` now exists: a
leaf is drawn by its OPERATION's name. For a workflow that is exactly
right — `charge`, `ship`, `notify` — and for a form it is useless,
because every field is the same operation and a term of them draws as
"ask, ask, ask". Inside a `direct` block the name comes from the
FUNCTION the author called, so a form written there names its fields
only by naming its helpers after them: the picture says `yourName`, not
`name`. Where the fields are DATA rather than code — which is what a
schema-derived form is — there is nowhere to put one def per field, so
the name had to become a parameter. `asking(name)(q)` is that, `ask` is
one line over it, and the test asserts BOTH pictures so the difference
is written down rather than discovered again.

TestFormProc (5), in okay-ui because the consumer is a page.
