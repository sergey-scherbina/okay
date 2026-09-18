## federation-refusal - a party runs what its owner allowed, for a coordinator its owner knows

Stage 1 of specs/federation.md showed two parties computing one answer
with neither one's records leaving. It did so with workers that run
ANY job their build knows, for ANYBODY who asks — fine between
processes one person started, and the entire question between
organisations. `Cluster.guarded(jobs, coordinators)(caller)(base)` is
that door: a `Serve` wrapper that answers `Resp.Failed` before the
request reaches the job.

THE ORDER OF THE TWO CHECKS IS THE INTERESTING PART. The coordinator
is checked first, so an unrecognised caller learns only that it is not
recognised — never which jobs the party allows, which would be a
directory of its business handed to whoever knocked. The test asserts
that ABSENCE, not just the refusal. It also asserts the party's log
counted zero reads: the refusal comes before the job, which is the
same rule stage 1 found for a foreign partition (emptiness is not a
refusal, and neither is a read that happened).

THE CALLER IS A PROPERTY OF THE CONNECTION, NOT OF `Req`. An identity
in the message sits where the sender controls it; a socket
authenticates once and every request on it comes from the party that
authenticated. So the wrapper takes it and the protocol is unchanged.
This is the AUTHORISATION half and it is deliberately dull — a set
membership test in front of a door that had none. Authentication stays
`okay-security`'s: which `Capability` a party accepts as an identity
is stage 3's question, at the door, with `Compat` beside it.

`Advance` and `Close` name a SESSION and not a job, so their job check
happened when the session was opened; the coordinator check still runs
on every request, and a test pins that a stranger cannot advance a
session somebody else opened.

STAGE 1'S LAST BOX TOO, and it was not the three-line test the spec
predicted. A party that resumes must resume from ITS log at ITS
position. The sink here is WINDOWED, so before dataflow's box 2b it
could not seek at all — and once it could, the test still showed party
0 re-reading all 10 000 of its records, because `PartyJob`'s flow was a
`Flow.of`, which skips by READING. The seek was real and the saving
was nil. A party's log is a topic and an offset is a number:
`Flow.seekable` closes it, and the test asserts each party reads more
than nothing and less than its whole log, with the horizon marks in
the journal.

What the controls said: with both checks disabled the three refusal
tests fail, and one of them fails with `no job named 'test.window' in
this build; it knows Vector(test.party)` — the worker's own
unknown-job message names every job the build knows. To a recognised
coordinator that is helpful; to a stranger it is the directory. With
the guard in front, a stranger never reaches it.
