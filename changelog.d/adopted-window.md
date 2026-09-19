## adopted-window - "has no window at all" was false, and now the window has a name

The operator asked why the previous lane filed the `growing-channel-order`
mechanism without proposing a fix. The rule it cited is real - hot-path
changes get priced here before they land, and the BUGS.md retraction
declined a fix for exactly that reason - but what got written was
"two candidate fixes, both hot path", which is a hand-wave standing
where a design should be. Reading `AdaptiveFifo` replaced it.

THE FIX IS ALREADY THERE, AND ITS COMMENT OVERSTATED IT.
`popManyAdoptedFirst` reads the adopted part 0 first whenever it has
anything in it, deliberately as a RULE rather than a one-shot phase -
the header explains that correction and then ends "and has no window
at all". It has one, and the code shows it in three lines:

    if took > 0 then ...
    else if open.get == 1 then 0
    else popManyScanning(max)(sink)   // part 0 was EMPTY

The rule holds per CALL, and the call is not atomic. Part 0 comes up
empty, the consumer goes off to scan other parts, and a straggler
whose route was read before the swap lands in part 0 while that scan
is running. Its element is delivered behind its own successors - the
same shape the rule was written to fix, and the shape the probe
measured: `1, 3, 7, 9, 11, 13, 15, 5, 17` at round 6303 of 40 000.

The sentence is corrected in place rather than deleted. The rule IS
the improvement it claims to be; only its last clause was false, and a
comment that overstates a guarantee is worse than one that admits a
window, because the next reader stops looking.

WHAT THE ENTRY SAYS NOW, instead of two vague candidates: seal part 0
to pushes at adoption, so a straggler is refused and reroutes to its
own part, landing after its predecessors. And TWO CANDIDATES THE TRACE
REFUTES, written down so nobody re-tries them: merging `grown` and
`inner` into one atomic changes nothing (the straggler's push into the
ring is legitimate until the swap), and "drain part 0 empty before
reading others" changes nothing either (the consumer DID see it empty
- that is the window). Plus the option that is the operator's and not
an agent's: weaken the documented promise to "per-producer FIFO except
across the one-shot swap", which is cheaper than any fix and gives
something up.

WHO STANDS ON THE PROMISE, surveyed because the operator asked what a
weakening would cost and the answer should be in the record rather
than in a chat. `TestChannelLaws` holds it as a law. `Channel.scala`
already gave up exact FIFO ACROSS producers on purpose and kept this
half, with `Queues.strong[A].fifo` as the escape hatch. `Source.merge`
RESTATES it to its own callers, so weakening means editing that too
and the streaming stack goes through it. okay-persist is NOT in the
decision - the durable journal writes through a JDK `FileChannel`.

AND THE ACTOR MAILBOX IS THE UNGUARDED ONE: a mailbox is a
`Channel[M]`, `Actor.scala` promises nothing about order, and "from
one sender, in send order" is what every reader of an actor model
assumes anyway. A weakening there would be silent, which is the worst
shape a weakening can take.

The shape of the defect argues the same way. One element displaced
across a one-shot swap, rare enough to need thousands of loaded
rounds: a user cannot reproduce it, will not connect it to this, and
will look in their own code. A promise broken once in thousands is
worse than no promise, because people lean on it precisely because it
is almost always true.

Still no behaviour change. What is different is that the next person
starts from a named window, one live candidate, two dead ones, the
three benchmarks that would price it, and the list of who is standing
on the answer.

Gate: a comment and the boards.
> **New entries live in [`changelog.d/`](changelog.d/), one file per
> landed lane** (changelog-d, 2026-09-18) — a landing writes its own
> file instead of the head of this one, so two lanes landing in the
> same hour no longer conflict. Read them with
> `scripts/changelog.sh`, or the whole history with
> `scripts/changelog.sh --all`.
>
> Everything below landed before the switch. It is the archive and is
> not edited again; its order is the one thing it carries that a
> directory of filenames cannot, which is why it was not split.
