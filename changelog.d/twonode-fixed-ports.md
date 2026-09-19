## twonode-fixed-ports - TestTwoNode tagged Live

`TestTwoNode` spawns two real JVMs on hardcoded ports (18091/18092),
so two agents gating at once could collide — one run got past
readiness and then died on a later request, evidence that something
else was already listening on those ports.

Priced both of the entry's fixes: an ephemeral port needs the child
to report its bound port back to the parent, real work since stdout
is currently `DISCARD`ed and the readiness poll would need it before
it can even start. `Live` — the cheaper fix, and the one its two
neighbors (`TestChatDemo`, `TestRepoAgent`) already carry — moves it
out of the default gate into `sbt integrationTest`; checked it still
runs and passes there.
