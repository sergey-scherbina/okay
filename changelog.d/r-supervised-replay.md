## r-supervised-replay - R's programs as data survive its respawn

The one piece of stage 6 (reliability) of specs/polyglot-one-wire.md
that R did not have: `RSubprocess` has always replaced R after a timeout,
but a program-as-data run's continuations died with the killed R.

- okay-r `RSubprocess`, the same road `SupervisedWorker` takes for the
  ForeignWorker family:
  - it renames each continuation the caller holds and remembers its run,
    its path of answers and the operation it stands at;
  - on a fresh R it re-runs the program and replays the path (a
    `generation` counter says which R a continuation lives on);
  - a program that is not pure is `ReplayDrift`, and a step that itself
    timed out is redone once.
- Test, live (docker R): R killed between two choices of a multi-shot
  program (a call past the deadline, from the callback), and all four
  branches come back. Mutant: without the replay, the branches answer
  `Left`.
- Docs: docs/modules/okay-r.md. Spec: polyglot-one-wire.md stage 6
  results.
