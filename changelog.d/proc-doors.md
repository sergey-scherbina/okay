## proc-doors - the last of the workflow arc, and a door built and removed

Stage 3 of specs/proc-notation.md, which closes the arc: every spec of
the workflow family now has zero open boxes that are not gated.

BOTH BOXES ARE MET AND ONE IS MET BY A REFUSAL. `!timer(t)` on its own
line has always compiled - the mark makes it a leaf. The COLOURED
spelling, `timer(t)` alone, cannot, and the reason is the rule the
whole feature rests on: auto-colouring fires where an ANSWER is
expected, and a statement expects nothing.

THE DOOR THE BOX ASKED FOR WAS BUILT, MEASURED AGAINST THE REST, AND
REMOVED. `Question.asked` - transparent inline, gated on the
capability, ascription built in, exactly `Direct.tell`'s shape - works
as Scala and collides with the check that makes colouring safe: it
expands to `val _ = q.reflect`, the inliner leaves a `$proxy` binding
holding the question, and the stray-question check sees a question
nobody asked. Teaching the check which bindings are consumed by which
marks is more machinery than a spelling is worth.

So the refusal stays and NAMES this case among the three that reach
it. That is the better outcome rather than a consolation: a compile
error saying "mark it" beats a door that compiles and a warning nobody
reads, which is what the half-built version produced - E176 from the
typer, before the macro ever sees the statement.

AND THREE BOXES WERE MARKED REFUSED RATHER THAN LEFT OPEN. Stage 2's
IR ("one front end, four back ends") is not earned: the arrow road
shares twenty lines with the other two and nothing else, because a
statement NESTS A CONTINUATION there and APPENDS TO AN ENVIRONMENT
here. The boxes are kept unticked as the shape a fourth road would
have to want.

Two stale backlog items closed with it: `workflow-operations` listed
the seven things stage 4 shipped on 2026-09-17, and `dialogue-patch`
said "what is left is only retirement tooling" - which landed the same
day as `Retire`. Both had outlived the work they described.
