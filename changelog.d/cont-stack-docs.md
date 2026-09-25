## cont-stack-docs — the user page for Cont and the stack

specs/cont-stack.md plan stage F (the operator's rule: a lane ships its
user docs). `docs/cont-stack.md`: the three kinds of shift body and
what each costs (a tail body is the value it passes, decided at compile
time; an answer-using body is a frame a level, counted, read exactly
where the platform allows, continued on a parked worker's stack past
the room); the per-platform table; `--enable-native-access` as the one
flag that matters for deep programs on the JVM, and that a library
cannot pass it for you; the knobs; the written bounds (an explicitly
smaller thread on the counted road, one frame over twice the worst
seen, the JS engine's stack); the measured costs; the literature. Two
examples, pinned verbatim in `TestDocExamplesContStack` — the deep one
on a 128 KB thread with the switch counter at zero. Indexed in
docs/README.md beside the continuations pages.
