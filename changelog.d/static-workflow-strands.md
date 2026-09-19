## static-workflow-strands - the deploy check, and a crash at every leaf

Stage 2 of specs/static-workflow.md. `Wf.Proc.strands(term)(x)(journals)`
answers which live runs a term would strand and where — a PURE
function, no row and no runtime, which is what lets a deploy ask ten
thousand journals whether they still fit the code about to ship BEFORE
shipping it rather than during.

THE DEPLOY CHECK IS TWO QUESTIONS, NOT ONE, AND WRITING THE TEST SAID
SO. The stage was specified as "a v2 term refuses the v1 journal". It
does not, and the reason is the doctrine working as designed: a journal
holds ANSWERS, so two author questions of the same answer type are
indistinguishable in it and a term that inserts one before them reads
every old answer one place across. `walk` catches a SHAPE - a clock
reading where an author's answer sits - and the envelope's `program`
field remains the mechanism for the other half. The test asserts BOTH,
including the mis-mapping that is NOT caught, because a limit nobody
writes down is a limit somebody finds in production.

THE EXHAUSTIVE CUT IS A PROPERTY, which is most of why the static
shape exists. "A crash resumes correctly" is normally a sample;
finitely many leaves make it a loop. Crash at every one over a REAL
topic, resume, compare: every activity runs once except the one the
crash caught in flight, which runs twice. The at-least-once floor with
a window of exactly one call, measured.

THE FIRST CUT OF THAT TEST MODELLED THE WRONG CRASH - it threw before
counting the activity, which is a process that died WITHOUT making its
outside call. The window is that the call happened, the card was
charged, and the answer never reached the log.

AND TWO MACRO BUGS FELL OUT, both invisible to every earlier test. A
projection typed as the path-dependent `env._2` unifies with a
reference type by luck and not with a primitive, so a block binding a
`Long` failed at the splice; and the answer type of a leaf is NOT the
last type argument, because a GADT case carries its own parameters
(`Question.Now[Q, A]` extends `Question[Q, A, Long]`). Every earlier
test went through a door whose declared type was the parent, and every
earlier slot was a reference — two coincidences holding the encoding
up.
