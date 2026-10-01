## cont-frames-register-pressure - `st` in a per-run cell instead of a loop register: refuted, nothing landed in code

- The segmented machine's install/pop gap (~1.36x the single list) is
  C2 keeping its three loop registers in stack slots (hsdis). The
  experiment moved `st` into a mutable per-run cell so the loop carries
  two, `focus` and `fs`, with one claim for the cell's indexes.
- It was WORSE: KontBenchmark.kontResetOnly 1.47-1.54x against the
  single list (1.39-1.42x with `st` a register). C2's loop grew to 930
  instructions, the `sp` loads stayed (68), and every write to the cell
  is a GC write barrier at every segment edge. Not committed; the rows
  are in history.d, and backlog cont-frames-register-pressure records it
  beside the earlier refutation (cold arms out of the loop).
