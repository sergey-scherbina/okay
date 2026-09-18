## static-workflow-optics - optics on a step, and a term that draws itself

Stages 3 and 4 of specs/static-workflow.md.

STAGE 3 BUILT NOTHING, WHICH WAS THE CLAIM. An optic is a function
polymorphic in a profunctor constrained by what it needs - a lens asks
for `Strong`, a prism for `Choice` - and `Proc` has had both since
stage 1, so `lens(step)` type-checked before this lane existed. What
the lane adds is whether it BEHAVES, which is a different question:
the step sees only the focused part, the surrounding state comes back
untouched, the journal holds the step's answers and nothing else, and
the term folds that journal back to the same whole. A prism's absent
case asks nothing and journals nothing, while `leaves` still reports
its step.

STAGE 4 IS A PICTURE DRAWN FROM THE TERM, and that is the whole
feature. A monadic engine can only draw a process from a status
projection somebody keeps in step with the code by hand, so the
picture and the program drift and the picture is the one nobody
checks. Here they cannot disagree - `Proc.mermaid` renders the same
value the engine runs.

Both sides of a choice are drawn, because which one runs is decided by
a value that does not exist yet. A loop is drawn ONCE with a back
edge: how often its body runs is not a fact the term has, and a
picture that unrolled it would be inventing a number. Pure steps are
not drawn. `mermaid(Some(path))` marks a position taken from `walk`,
so a dashboard draws where a run stands without replaying it.

Seen failing: dropping the bypass edge for a choice's untaken side
reddens the test that exists for it.

WHAT WAS NOT DONE IS NAMED rather than quietly ticked. The third box
of stage 3 asked for `ui-direct-example`'s form as a `Proc`; its two
claims are already made by the tests above and by TestProcCut, so what
a form would add is a CONSUMER, which belongs to a UI lane. Filed as
`proc-form-consumer`.

docs/static-workflows.md gained both sections, with a generated
picture rather than a hand-drawn one.
