# schema-stubs — the other side's declarations, from the same Schema

## Overview

A value crossing to Python or to TypeScript is typed on ONE side only:
okay checks it against its `Schema` at the boundary, and the other
language never sees the type. A renamed field is found when a call fails,
not when the other side's type checker runs. This generates the other
side's declaration from the same `Schema` the codecs fold, so the other
side's type checker can see the type too (backlog polyglot-schema-stubs).

Each generator follows the CODEC that carries the value, because a
declaration is only true if it matches the wire:

| target | carried by | a product | a sum | `None` | `BigInt` | bytes |
|---|---|---|---|---|---|---|
| Python `TypedDict` | okay-py's `PyCodec` | a dict of its fields | the case's dict plus `"type": Literal["Case"]` | `None` | `int` | `bytes` |
| TypeScript `.d.ts` | okay-codec's `Json` | an object | `{ "Case": {...} }`, externally tagged | `null` | a string of digits | a base64 string |

## Behavior

- [x] `Stubs.python(schemas*)`: a Python module (`from __future__ import
      annotations`, `TypedDict`, `Literal`, `Optional`, `Union`) declaring
      every product reached as a `TypedDict` and every sum as a `Union` of
      its cases, each case carrying its `type` literal. Declarations come
      out in dependency order, once each, recursive types included.
- [x] `Stubs.typescript(schemas*)`: `export interface` per product,
      `export type` per sum as a union of single-key objects, `T | null`
      for an option, and a comment where the JS number cannot hold a
      `Long` exactly.
- [x] Deterministic output: the same schemas give the same text.
- [x] Checked by the REAL checkers (Live): mypy (through `uvx`) accepts a
      Python function that reads the generated `TypedDict` correctly and
      REJECTS one that reads a field that does not exist; `tsc` does the
      same for the `.d.ts`. And the value okay actually sends is the value
      the declaration describes: PyCodec's encoding of a sample passes
      mypy's check as a literal of the declared type, and Json's encoding
      of the same sample is assignable to the TypeScript type.
- [x] R, Clojure and Frege are left out, and said so: R has no types to
      declare; Clojure's malli and Frege's `data` would be declarations of
      JVM values those languages already read through okay's own classes.

## Decisions

- **`TypedDict`, not dataclasses, for Python.** A case class reaches
  Python AS a dict. A dataclass would need a conversion at every
  function; a `TypedDict` describes the dict that is already there, and
  a checker narrows a union of them on the `"type"` literal.
- **One generator per CODEC, not per language.** The TypeScript output
  follows `Json` (externally tagged sums, digits for a BigInt) because
  that is what an okay HTTP API or a Scala.js export sends. A Python
  declaration shaped like the JSON would describe a value Python never
  receives.
- **Name clashes.** A case whose name is already declared is prefixed
  by its sum (`ShapeCircle`). A standalone product declared AFTER a sum
  case of the same name reuses the case's declaration: rare, and stated
  here rather than solved.

## Results

- 2026-09-23. TestStubs (4, in the default gate, all platforms) pins
  the shapes. TestStubsTsc (Live, `tsc --strict`) and TestPyStubs (Live,
  `uvx mypy --strict`) check what the codecs SEND against the
  declarations, and a read of a field that does not exist is refused.
- The mypy check failed on its first run, correctly. PyCodec sent a
  `BigInt` past a Long as a `str` (`Str(digits)`), where the declaration
  says `int`. The wire already carried unbounded ints (the `"int"` tag),
  but the host had no value for one. `PyValue.BigI` now carries it: the
  codec was fixed, not the stub.
