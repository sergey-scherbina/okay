## optics-outside-query - a query two interpreters read, with a law against SQLite

The operator lifted the wait ("Все это нужно", 2026-09-23). The spec
had ranked this fourth because typed query DSLs are a swamp; what
landed takes only what the criterion asks for.

- okay-sql `Query`: `Query.field[A, T](name)` — a field by NAME
  checked against `Schema[A]` at construction (refused by name, and
  by column TYPE through `Typed`); `===`/`=!=`/`<`/`<=`/`>`/`>=`/
  `like`/`isNull`, `and`/`or`/`unary_!`. `Where[A].sql` DESCRIBES
  (the clause and its parameters, columns as `Typed` names them),
  `fields` audits, `test(a)` RUNS the same predicate in memory over
  the value's `Typed`-bound fields, NULL three-valued. `select`
  renders what `Typed.rows[A]` decodes; `update` renders the UPDATE
  and applies the same edit in memory through the codec.
- THE LAW against a real engine (`TestQuerySqlite`, okay-jdbc,
  embedded SQLite in the default gate): fourteen predicates, the
  engine's rows equal `test`'s; an UPDATE leaves the table as the
  in-memory edit leaves the rows. The one divergence is a test that
  says so: SQLite's ASCII-case-insensitive `LIKE`.
- `Typed.columnOf`/`typeOf` (`private[sql]`) for the type check.
- Filed at the operator's word: `free-return-rename` (the answer node
  `Free.Pure` → `Free.Return`; the row and `pure` stay) and
  `bind-in-row-union` (a bind over the union of two rows, with the
  ergonomics questions to answer first) — both in the sprint queue.
- specs/optics-outside.md stage 9; docs/optics.md §7; the okay-sql
  module page.

Gate `affected master` green, no warnings.
