## okay2-tables - the table layer in okay2-stream

okay-stream's table layer, as new files in okay2-stream:
- `Bulk[D]`, with `Bulk.local` over `Chunks`;
- `Csv`;
- `Tables`, an effect that builds a first-order `Plan` on a heap in
  State. Its rewrites push a projection into the read and put the
  smaller side of a join on the right. `via`/`run` work on any `Bulk`;
- `Sort` via the primitives.

Each operation and plan node translates itself, so the only cast is the
core's own heap-slot cast (specs/okay2.md stage 28). 13 tests.

Docs: docs/okay2.md section 31.
