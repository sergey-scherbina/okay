## order-choice - the default says what it keeps, and offers the two that keep more

The operator's decision, after the mechanism was named: weaken the
promise to what `growing` actually delivers, and put the guaranteed
variants in front of the people who need them.

WHAT THE DEFAULT PROMISES NOW. `Channel(n)` is the `growing` buffer:
one ring until a second producer appears, then that ring is ADOPTED as
part 0 and everyone gets a part. A producer whose elements straddle
that one-shot swap can have its own order broken **in at most one
place, once**. `TestChannelLaws` states exactly that and the gate line
says which claim it checked — "EXCEPT once, across its one swap" — so
a reader of the output cannot mistake the weaker guarantee for the
strong one.

THE WEAKENED LAW IS NOT A LICENCE. It asserts at most ONE inversion
per producer, no duplication and no invention. The mass reordering
this buffer had before `popManyAdoptedFirst` - 73 rounds in 300, a
source coming back `1..16, 49, 50, 17..48` - is MANY inversions and
still fails. Every other mechanism in the table still signs for the
exact law, which is why the weakening is a named set of one rather
than a lower bar for everybody.

THE TWO THAT KEEP MORE ALREADY EXISTED, and the lane's real work was
making them findable:

| what you have | what to write | what you get |
|---|---|---|
| one producer, or order between senders decides nothing | `Channel[A](n)` | the default; one displacement across the swap |
| many producers AND one producer's order means something | `Queues.strong[A].adaptive.each(n).build` | exact per-producer order - it never adopts, so there is no swap; 38x faster than a ring at sixteen producers, 19% slower at one |
| exact order across ALL producers | `Queues.strong[A].fifo(n).build` | one tail, one CAS, total FIFO |

ACTORS GET THIS TWICE, and that is the point of the lane. A mailbox is
a `Channel[M]` and the default is the growing one; "messages from one
sender arrive in the order it sent them" is what every reader of an
actor model assumes without being told. A weakening documented only on
the buffer's page would have been silent. `ActorRef`'s own header now
carries the choice, the rule of thumb (independent senders - the
default; a protocol whose messages mean something in sequence -
`adaptive`), and the three spellings.

AND THE SPELLINGS ARE COMPILED AND RUN. `TestMailboxChoice` spawns an
actor on each of the three mailboxes. It earned its place immediately:
the first cut of the header said `Queues.strong[M].fifo(256)`, which
is a BUILDER and not a channel, and nothing would ever have caught it
because a comment is not code.

BUGS.md's `growing-channel-order` is closed as a documented trade
rather than fixed. The optional fix stays filed as
`growing-order-drain-guarantee`: it is no longer a bug to chase, it is
a guarantee somebody may decide to buy back with a measurement.
