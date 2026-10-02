# shift-merge — Delim and Shift, one effect named `Shift`

Status: spec, 2026-10-02. Owner lane: `shift-merge`. Sprint
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

- [ ] stage 1: every `Delim` suite green renamed onto `Shift % ?`
      (TestDelim*, the collect/resumable/replay suites), TestShift and
      TestShiftPatterns unchanged; a static program widened with
      `Shift.dynamic` mixes with a dynamic one in one `flatMap`
- [ ] the machine guard: a row holding any `Shift` refuses a second
      machine at compile time, with today's message
- [ ] stage 2: the satellites' suites green; no `Delim` left in code
- [ ] the differential oracle (TestDelimitedDifferential) and the
      machine's depth suite unchanged and green — this lane changes the
      front, not the machine

## Decisions

- **The name is `Shift`** (operator), the dynamic form `Shift % ?` (the
  operator's glyph, its literal Scala spelling).
- **No `type Delim` alias.** Two names for one effect is what this lane
  removes; the 17 files move in stage 2.
- **Rows stay invariant**: static → dynamic is a written coercion
  (`Shift.dynamic`), as every row widening in okay is (widen-is-a-
  coercion).
