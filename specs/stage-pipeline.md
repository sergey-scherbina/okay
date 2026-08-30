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
- [ ] through is associative; the identity stage (await-tell loop) is
      its unit
- [ ] a finite downstream ends an infinite upstream through any number
      of stages (laziness through composition)
- [ ] flush: a stage emits buffered output on end-of-input before
      finishing
- [ ] chunked/unchunked adapters compose with through and preserve
      element order and content
- [ ] effectful stages (`+ Async`, `+ G`) forward through composition
      (handlers remain stream transformers)

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
