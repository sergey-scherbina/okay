# okay-graphql: GraphQL on the kernel

## Why

A GraphQL server's hard problem is not the syntax. It is that a query fans out: `users { orders { items } }`
asks, field by field, for data that a naive server fetches one row at a time (N+1). The usual cure is a
DataLoader written by hand beside every resolver. On the kernel the cure is structural: a resolver is a PROGRAM
of an effect, `Fetch`, and the kernel can run a program to its next operation and hand back the rest of it as
data (`p.step`: `Step.Yield(op, in, resume)`). An executor that holds many such suspended programs at once sees
all their fetches together, sends each source ONE request, and resumes every program with its answer — Haxl's
idea (Marlow et al., "There is no fork", ICFP 2014), with resolvers written as plain sequential code.

## The pieces

- `Json` — okay-codec's, for responses and variables. No new JSON.
- `Document` — an executable document, parsed by our own parser behind a facade (`GraphQLParser`, ours the
  default; an adapter over graphql-java is backlog, optional). Operations (query, mutation), variables with
  types and defaults, selection sets, fields with aliases and arguments, named fragments, inline fragments,
  `@skip` / `@include`. Values: int, float, string, boolean, null, enum, list, object, variable.
- `Schema` — Scala values, not annotations: `ObjectType[A]` with fields, scalars (`Int`, `Float`, `String`,
  `Boolean`, `ID`), `ListOf`, `Nullable` (non-null is the default: a Scala value is there), enums by name,
  arguments decoded from GraphQL values. A field's resolver takes its parent and its arguments and returns a
  program: `B ! Fetches` (`Fetches = Fetch +: Pure`). `schema.sdl` prints the schema.
- `Fetch` — the effect of data: `Fetch.get(source, key)`, a `Source[K, V]` answering MANY keys in one call.
  Its applicative words: `Fetch.both(a, b)`, `Fetch.all(ps)`: independent fetches of one resolver in one round.
- `Executor` — `execute(schema, document, variables): Json`, the GraphQL response (`data`, `errors`).

## How it runs: rounds

A query is a tree of fields; the executor runs it as a set of TASKS, each a resolver's program stepped by the
kernel. One round:

1. Step every runnable task: a task ends with a value, or stops at a `Fetch` operation (`Step.Yield`).
2. A value completes its field: a scalar is written into the result; an object's selected fields become new
   tasks, runnable THIS round; a list's items each the same. So everything that can run without data runs now.
3. When nothing is runnable, the round's fetches are grouped by source and each source is called once with all
   its keys (duplicates once). Each waiting task is resumed with its value and becomes runnable: the next round.

Fields of an object and items of a list are the applicative product: independent, so their fetches meet in a
round. `Fetch.both(a, b)` / `Fetch.all(ps)` give a resolver the same for its own independent programs: the
executor steps the parts as tasks of their own and resumes the resolver with all their values. A sequence in a
resolver (`for a <- f1; b <- f2(a)`) is a dependency, and stays one round after another — as it must.

The number of calls to a source is the number of rounds that need it, not the number of fields: 100 users and
their orders are two calls, not 101.

## Errors and nulls

A resolver that fails (a raise, a throw, a key its source did not answer) is an error at its PATH
(`["users", 3, "orders"]`), its message in `errors`. The field becomes null; if its type is non-null the null
goes up to the nearest nullable parent (the spec's null propagation), up to `data: null`. A document that does
not parse or names a field the schema lacks is a request error: `errors` only, no `data`.

## Mutations

Top-level fields of a mutation run IN ORDER, each to completion (its own rounds) before the next starts, as the
spec requires; below the top level a mutation's selections run in rounds like a query's.

## Bounds (no unbounded recursion)

The parser's nesting (selections, values) and the executor's depth are bounded by `Limits.depth` (default 64): a
deeper document is an error, not a stack overflow. Rounds are a loop; tasks a work list.

## Not in this lane (backlog)

Introspection (`__schema`, `__type`; `__typename` is in), subscriptions (a stream: okay-stream), an HTTP route
(okay-http), validation beyond unknown fields and arguments, an adapter over graphql-java, a source answering
with a program (async data sources): `Source.fetch` is a plain function here.

## Behavior

- [x] a query of scalars, nested objects, lists, nullables
- [x] arguments and variables (with defaults), aliases
- [x] named and inline fragments, `@skip` / `@include`, `__typename`
- [x] batching: N fields fetching one source are one call per round; duplicates once
- [x] `Fetch.both` / `Fetch.all`: one round for independent fetches inside a resolver
- [x] errors with paths; null propagation through non-null fields
- [x] mutations: top-level fields in order
- [x] request errors: a parse error, an unknown field
- [x] depth bound; a list of 100 000 items in constant stack
- [x] `schema.sdl`

## Decisions

- Batching by STEPPING, not by an applicative data type of our own: resolvers stay programs of the kernel, the
  executor drives them with `step` (Step.Yield) — the operation and the rest as data. Considered: a Haxl-style
  `Fetch[A]` with `ap` (Done | Blocked); refuted as a second monad beside the kernel's.
- Parallel fields are the applicative product (operator, 2026-10-15): fields and list items are independent by
  the shape of the query, so the executor treats them as one product; a resolver asks for the same explicitly.
- The source map of a round is keyed by identity and holds batches of many key and value types: ONE cast,
  `Run.batchOf`, says why the batch under a source is that source's. Everything else is typed — the operation by
  `In.same` against `In.Here[Fetch, Pure]` (no type test), `Fetch.Get`/`All`/`Both` by GADT matching.
- No recursion through continuations: every continuation (a task stepped, a fetch answered, a join met, a field
  or item completed) is ENQUEUED on the run queue; `rounds` is the one loop. Measured: 100 000 list items and
  100 000 fetches in one round, constant stack (TestGraphQL).

## Results

- TestGraphQL, 14 tests, the behavior list above. The shop: `{ users(first: 100) { orders { id user { name } } } }`
  is one call to `orders` (100 keys) and one to `users` (each key once) — two source calls where a resolver per
  field fetching row by row makes one per field.
- An error belongs to the program that failed. `Fetch.traverse` over keys one of which the source left out fails
  its one program — the whole field. Per-item errors (operator, 2026-10-15: "это нужно исправить"): `Fetched(of)`,
  an output type whose value is a program, run as a task where it stands — `ListOf(Fetched(Nullable(userT)))`, a
  resolver returning a list of programs: the items' keys still one call, a missing item null at its index with
  its error at `["byIds", 1]` (TestGraphQL). Considered: catching per key inside `traverse` — refuted, a program
  has one outcome, and a partial list is not a value of `List[B]`.
