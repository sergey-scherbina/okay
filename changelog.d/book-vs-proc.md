## book-vs-proc - the book argued the static route was impossible; it landed the next day

`docs/continuations/appendix-a-if-you-really-want.md` spends a section
on why a durable workflow cannot have a static shape, and its example
is a loop:

    val nights = !w.pause("how many nights?")
    for _ <- 1 to nights do !w.pause("which room?")

"because the *shape* now depends on an answer". `static-workflow` and
`proc-notation` landed that loop the next day. So the repository
carried two documents that contradicted each other, and the older,
more confident one was the book.

THE ARGUMENT WAS RIGHT ABOUT `Selective` AND TOO WIDE. `whileS` is a
recursive DEFINITION, and in a strict free structure that is an
infinite term - which is the whole of why the shape could not depend
on an answer. Elgot iteration is a NODE, so the term stays finite and
the trip count is a value. The section now carries the refutation, the
working loop, and a third row in its trade table:

    arrow (Proc) | a PATH into a finite term, with a counter per loop
                 | replay, and walk re-derives the position with no runtime
                 | anything short of ArrowApply

What the arrow rung still cannot do is the thing Hughes proved: run a
step chosen by a value the program binds. So "the shape may depend on
an answer" is true for branching and iteration and false for WHICH
PROGRAM RUNS NEXT - a much smaller loss than the section claimed.

THE MISTAKE IS LEFT IN PLACE rather than edited out, with why: the
argument was sound and the conclusion too wide, because it reasoned
from ONE rung of the ladder to every rung below the monad. That is the
most ordinary way to be wrong about a design, and the fix was not
cleverness - it was noticing that the literature's `whileS` and an
iteration node are different things.

Also corrected: the appendix's closing advice sent `if paid` and
`while retries < n` to the monad, and its verdict said there were two
halves. There are three, and the arrow is the middle one. Chapter 23
gained a section pointing at docs/static-workflows.md.

FOUND BY ASKING "did you write the documentation?" and checking rather
than answering. A new page had been written; the book that argues the
opposite had not been read since.
