- [x] dialogue-patch — DONE 2026-09-17 in core (dialogue-asks) and
      through the log (wf-durable-journal, `Dialogue.workflow`). What
      is left is only retirement tooling: something that says which
      programs are still present in a topic, so a branch can be
      deleted with evidence rather than hope.
      Was: stage 2: `patch(id)` (Temporal's `getVersion`),
      so a program that changed can carry its old runs to the end
      instead of stopping them. Stage 0 made the change a loud stop;
      this is how the stop goes away. Needs the `Patched` entry the
      envelope already has room for.
      AND THE RETIREMENT TOOLING LANDED TOO (workflow-retire,
      2026-09-17): `Retire.census` over envelopes alone, `states` by
      replay, and `patches` by replay WITH the body — because a patch
      id lives in the QUESTION and a journal holds answers, so no
      reader of records can recover it. Ticked late 2026-09-18.
