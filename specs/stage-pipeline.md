# Stage pipelines — coroutine transducers in the core

## Overview
A pipeline stage is a program that awaits inputs and tells outputs — a
transducer as a value. Tokenizers, parsers, codec dialects and any
stream rewriter share this one shape; composition is demand-driven
coroutine pairing (the downstream await drives the upstream), so every
stage is incremental, resumable from any prefix, and lazy by
construction. Lives in the CORE `okay` module (dependency-free); the
lex/parse/codec modules build on it.

## Interface
```scala
/** a stage: awaits I, tells O, finishes with A */
type Stage[I, O, A] = A ! Take % I + Writer % O

/** compose: s2's awaits are served by s1's tells, demand-driven */
def through[I, M, O, A, B](s1: Stage[I, M, A])(s2: Stage[M, O, B]): Stage[I, O, B]

/** run a producer through a stage (generalizes pipe) */
def through[W, M, A, B](p: A ! Writer % W)(s: Stage[W, M, B]): B ! Writer % M

/** chunked stages for throughput; rechunk adapters at the seams */
type StageC[I, O, A] = Stage[Chunk[I], Chunk[O], A]
def chunked[I, O, A](s: Stage[I, O, A]): StageC[I, O, A]   // adapter
def unchunked[I, O, A](s: StageC[I, O, A]): Stage[I, O, A] // adapter
```

## Semantics
- Demand-driven: nothing runs until the final consumer awaits; a
  stage's tells buffer at most until the downstream's next await (no
  hidden queues — the continuation IS the buffer).
- End of input: upstream exhaustion answers None to awaits (as pipe
  does today); a stage may still tell after seeing None (the FLUSH:
  emitting what its state holds — a partial token, a pending chunk)
  before finishing.
- A stage's answer A is its own (statistics, final state); through
  keeps the DOWNSTREAM answer, as pipe keeps the consumer's.

## Behavior
- [x] through is associative; the identity stage (await-tell loop) is
      its unit (behavior-tested on samples)
- [x] a finite downstream ends an infinite upstream through any number
      of stages (laziness through composition)
- [x] flush: a stage emits buffered output on end-of-input before
      finishing (Stage.chunked flushes its tail)
- [x] chunked/unchunked adapters compose with through and preserve
      element order and content (round-trip test)
- [x] effectful stages (`+ Async`, `+ G`) forward through composition —
      the throughG and throughProducerG overloads: either side's G ops
      forward into the composed row in the order the pull crosses
      them, laziness intact (a pure stage joins an effectful row via
      !.widen and a union-ACI ascription); associativity tested with
      effects in the row

## Phased stages (stage-phased) — typestate on the stream

A stream with PHASES (a header before rows — the CSV shape, a
preamble before frames) forces today's transducer to encode the
phase as a sum type in S, and every step carries branches for
states that are illegal in that phase. `Stage.phased` removes the
illegal states STRUCTURALLY: the accumulator CHANGES TYPE at the
switch, so the body step cannot even mention the header phase —
Atkey's parameterised composition applied to the pipeline, and
PState (the theory exhibit of docs/theory/03) gains its stream
consumer: the per-input transition is executed as a PState program,
the reified form of the type change the two loops enforce.

```scala
/** two phases, one stage: `head` consumes I at S1 and either stays
 * (Left) or SWITCHES (Right) carrying the S2 the body starts from;
 * `body` runs at S2 and never sees S1 — not by discipline, by type.
 * Ends are honest both ways: input may end DURING the head. */
def phased[I, O, S1, S2](z: S1)(
  head: (S1, I) => Either[(S1, Vector[O]), (S2, Vector[O])],
  body: (S2, I) => (S2, Vector[O]),
  endHead: S1 => Vector[O],
  endBody: S2 => Vector[O]): Stage[I, O, Either[S1, S2]]
```

Behavior:
- [x] the CSV shape: the head parses the header line into the
      column index (S2 = the names), the body emits typed rows
      keyed by it; outputs and the final Right(S2) agree with a
      hand-written run
- [x] outputs telled AT the switch (the Right's vector) arrive
      before the body's first output; order is total
- [x] input ending DURING the head answers Left(S1) and endHead's
      flush; ending in the body answers Right(S2) and endBody's
- [x] the illegal state does not COMPILE: a body step written
      against the head's type is a compile error, asserted with
      compileErrors — the first typestate proof in the suite
- [x] the transition runs through PState: the switch step is a
      Cont program whose state type changes S1 -> S2 (the Atkey
      instance, executed rather than exhibited)

## phased3 (stage-phased3) — the three-phase sibling

`Stage.phased` covers two phases; the http message shape
(request-line -> headers -> body) wants three. Not a family of
combinators: ONE more arity, because the consumer exists (http.md,
http-message-phases) and because chaining two phased stages cannot
express it — the middle phase's END is the third's typed start, and
`body` is a step function, not a stage. Same guarantees: no phase
enum, illegal states unrepresentable, ends honest in all three
phases, the transitions run through PState. ADDITIVE per the
adoption doctrine (specs/delimited-control.md): transduce stays,
phased/phased3 are the extra door.

- [x] phased3 drives the http message shape (the consumer's test);
      the two-phase law holds at each seam: switch outputs precede
      the next phase's, the answer names the dying phase (three-way)
- [x] the wrong-phase step is a compile error at BOTH seams

## Out of scope
- fan-in/fan-out topologies (Channel territory)
- own buffering/backpressure (Channel territory; a Stage never queues)

## Decisions
- **Stage as an effect-union type alias, not a class** — transducers
  are programs; all existing machinery (handlers, forwarding, chunked
  Writer observation, laziness contract) applies verbatim.
- **Multi-channel output** (e.g. instructions + diagnostics) is a
  union of Writers with class-distinct element types, or one Writer of
  a sum — decided per module (see streaming-parse.md); the core does
  not privilege either.
