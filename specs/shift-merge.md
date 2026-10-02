# shift-merge — Delim and Shift, one effect named `Shift`

Status: stages 1, 2 and 4 done, 2026-10-02; the one guard in progress (shift-merge-guard), stage 3 after it. Owner lane: `shift-merge`. Sprint
cont-js-depth, the design conversation after stage 3a.

## Why

`Delim` (Delim.scala) and `Shift` (Shift.scala) are ONE thing — capture
the continuation up to a delimiter, on the one machine (Delimited.scala);
`Shift` is even implemented as `Delim`'s operations re-typed. They
differ in two respects only:

| | `Delim` | `Shift` |
|---|---|---|
| how a delimiter is named | a `Prompt[R]` VALUE, made at run time, any number | the answer TYPE `R` (`Shift.Key[R]`, interned by type) |
| what the row says | "delimited control here" (`R ! Delim + F`): a capture to a prompt not installed is `NoPrompt` at run time | "a capture to the `reset` of `R`" (`Shift % R`): `reset[R]` is its handler, so the compiler refuses a program with an unhandled capture |

Besides: `Delim.Stacked` tracks value prompts at the TYPE level
(`p.type` on a type-level stack) — a third face of the same thing. Two
names, two machine guards (`OneMachine`, `Nesting`), five delimiter
handles, and `shift`/`reset` spelt in seven places. `Shift` landed today
(shift-effect-core) and nothing outside the core uses it; `Delim` is used
by 17 files in 8 modules. So `Delim` is folded INTO `Shift`, and the name
is `Shift` (operator).

## The one effect

```scala
sealed trait Shift[K, +A]          // K: the delimiter's KEY, a type
```

Every form is `Shift % K` in the row; the forms differ by the key:

| key `K` | delimiter chosen by | today | `NoPrompt` possible |
|---|---|---|---|
| an answer type `R` | the type: the nearest `reset` of `R` | `Shift % R` | no — `reset[R]` handles it |
| a prompt's singleton type `p.type` | that prompt value, tracked statically | `Delim.Stacked` | no |
| `?` (any) | a `Prompt[R]` value at run time | `R ! Delim + F` | yes, and the type says so |

**`Shift % ?` is the operator's glyph for the dynamic form** ("`Shift %?`
meaning `Shift % Any`"): Scala types have no postfix operator, and the
wildcard is the literal spelling — `%[Shift, ?]` reduces to
`[A] =>> Shift[?, A]`, "a Shift with some key". PROBED (this lane): the
spelling compiles in signatures, in an alias and generically. A static
program does NOT enter `Shift % ?` by subtyping — rows are invariant in
`!` (free-row-invariance, measured) — but by the ordinary row coercion,
`Row.into[A, Shift % R + F, Shift % ? + F]`, after which programs of
different keys mix in one `flatMap`. `Shift.dynamic(p)` names it.

## Names (one set)

- Top level, the static form keyed by the answer type (today's `Shift`,
  the most common and the safe case): `shift`, `shift0`, `reset`, and
  `Shift.exit`, `Shift.collect`/`emit`.
- `object Shift` — everything `Delim` has, row `Shift % ? + F`:
  `prompt`, `push`, `dollar`, `shift`/`shift0`/`abort` at a `Prompt`,
  `reset(p => …)`, `scope`/`delimited` (`Prompted`), `collect`/
  `collecting`/`collectUntil`/`collectingUntil`/`emit`, `resumable`/
  `pausing`/`pause`/`ask`/`drive`/`answer`/`replay`, `onReturn`,
  `run`/`runNested`, and `Stacked` (the `p.type` form).
- `Prompt[R]` and `NoPrompt` stay as they are.
- ONE machine guard: "a machine already runs in `F`" = `F` holds some
  `Shift` (any key) — today's `OneMachine` (`Delim`) and `Nesting`
  (`Shift`) become one.
- `type Delim` is GONE, not aliased: every use moves to `Shift % ?`.

## Stages

1. **The effect and its doors** (core): `Shift[K, +A]`, `Shift % ?`
   carrying today's `Delim` operations (`Cont0`, re-typed at the doors
   as `Delim`'s `in`/`out` do now), `object Shift` gaining every `Delim`
   door, the one machine guard, `Shift.dynamic`. Every core caller moved
   (Lexical, Layered, Replayable, Delimited's docs, Effects, Cont). The
   static form unchanged in meaning.
2. **The satellites**: okay-persist, okay-direct, okay-workflow, okay-ui,
   okay-llm, okay-foreign-workflow and the rest of the 17 files —
   `Delim + F` → `Shift % ? + F`, `Delim.x` → `Shift.x`. Mechanical.
3. **`Stacked` as `Shift % p.type`**: the type-level prompt stack read
   through the same row as the other two forms.
4. **The page**: docs/continuations-in-practice.md and
   docs/delimited.md rewritten around one effect and its three keys;
   every `Delim` in docs/ (127 lines) moved.

## Behavior

- [x] stage 1: every `Delim` suite green renamed onto `Shift % ?`
      (TestDelim*, the collect/resumable/replay suites); TestShift and
      TestShiftPatterns pass with `exit`/`emit` from the ONE evidence; a
      static program widened with `Shift.dynamic` mixes with a capture to
      a prompt by value in one program (TestShift)
- [ ] the machine guard: ONE evidence, `Shift.Machine[F]`, for "a machine
      already runs in F" (lane shift-merge-guard; `OneMachine` and
      `Nesting` both gone)
- [ ] every machine-starting door (`run`, `delimited`, `collect`,
      `collectUntil`, `resumable`, `drive`, `answer`, `replay`, the keyed
      `reset`, `Stacked.run`/`delimited`) NESTS when a machine runs
      outside: it pushes its delimiter on that machine instead of starting a
      second — what `scope`/`collecting`/`pausing` do, now chosen by the
      row, so the "SECOND machine" compile error is gone (TestBookOneMachine)
- [ ] an ABSTRACT row is a compile error naming the fix, "take `using
      Shift.Machine[F]`": the hole chapter 12 demonstrated (`NotGiven`
      reading "unknown" as "absent", a generic helper swallowing the
      obligation) is closed (TestBookOneMachine)
- [ ] a row with a `Shift` AND an abstract part reads as nested (the
      `Shift` is certain); a row of only concrete non-`Shift` parts reads
      as outermost
- [x] stage 2: the satellites' suites green; no `Delim` left in code
      (okay2, a separate Scala 2 build with its own `Delim`, untouched)
- [x] the differential oracle and the machine's depth suite unchanged and
      green — this lane changed the front, not the machine
- [x] stage 4: every `Delim` in docs/ moved (docs/okay2.md excepted, it
      documents okay2's own); the pinned examples re-pinned

## Decisions

- **ONE BLOCK EVIDENCE, `Shift.Prompted`** (operator: "делай", after
  asking whether `reset` is the handler). A keyed `reset` and a
  `delimited`/`scope`/`dollar` both make it; it carries the prompt, the
  answer (`Res`), the key's effect (`K`: `Shift % R` or `Shift % ?`) and
  the row outside (`F`). `Shift.In` is gone. So `exit`, `emit` and the
  one-argument `shift`/`shift0` work in any block.
- **Their row is CHOSEN (`Shift.RowFor`):** inside a `direct` block the
  block's (a helper holding the evidence abstractly, `!Shift.emit(a)`,
  needs it — the evidence's own row is unknown there), elsewhere the
  evidence's `K + F` (a `for` over a `reset` reads it). A given with a
  priority, the direct one INLINE with an inline `DirectCtx`. Refuted on
  the way: an inline `summonFrom` on `DirectCtx[f]` (its type variable
  leaked into the block's typing: "Found DirectCtx[f], Required
  DirectCtx[Nothing]"), and a non-inline given holding the `DirectCtx`
  ("a reference to parameter contextual$2 was used outside the scope
  where it was defined" — the evidence exists only while `direct`
  expands).
- **The static `Shift.collect/emit/exit` of shift-patterns are the same
  names now**: one `collect`/`emit`/`exit` for every block.
- **ONE GUARD, AND IT NESTS INSTEAD OF REFUSING** (shift-merge-guard,
  operator "Начинай" on the plan "one evidence; it decides whether a block
  starts its machine or stands on the running one"). `OneMachine`
  (`NotGiven[Shift[?, Any] <:< F[Any]]`, a compile error at a `Shift`
  row) and `Nesting` (a macro reading the row, the keyed `reset` nesting
  on the outer machine) answered one question two ways. The macro's
  answer is the one kept, for every door: the error existed only because
  the dynamic doors could not nest, and they can — a nested door is its
  `scope`-form (`scope`, `collecting`, `pausing`) with the row narrowed,
  the same cast the keyed `reset` already made (`innerDyn`). What the
  two never caught, the macro now refuses: an abstract row has no answer,
  so it asks for the evidence as a parameter (pass the obligation on —
  chapter 12's rule, enforced instead of advised). Refuted: keeping a
  compile error for the dynamic doors "because the nested spelling is
  explicit" — two spellings of one block, chosen by a row the compiler
  can read, is the confusion this spec removes.
- **Noted, not changed here:** a keyed `reset` counts its nested runs in a
  `ThreadLocal` room and switches stacks past it (`runReset`); the
  cont-stack rule says a room is a value, never a `ThreadLocal`.

- **The name is `Shift`** (operator), the dynamic form `Shift % ?` (the
  operator's glyph, its literal Scala spelling).
- **No `type Delim` alias.** Two names for one effect is what this lane
  removes; the 17 files move in stage 2.
- **Rows stay invariant**: static → dynamic is a written coercion
  (`Shift.dynamic`), as every row widening in okay is (widen-is-a-
  coercion).
