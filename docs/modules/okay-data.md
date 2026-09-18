# okay-data

Data structures that are not the effect system: the approximate
aggregators, and the coordination-free pair underneath sortable
identity.

## What is here

| area | the types | its spec |
|---|---|---|
| approximate aggregation | `Sketch` — HyperLogLog, count-min, quantiles | [aggregators.md](../../specs/aggregators.md) |
| coordination-free identity | `Uid` (a sortable 128-bit id, ULID or UUID spelling) and `Hlc` (the hybrid logical clock under it) | [coordination-free.md](../../specs/coordination-free.md) |

Two themes in one module on purpose. Split them when either grows a
second file; until then two modules of 309 and 374 lines, with one
consumer group each, would be structure ahead of need.

## What stayed in the core, and why it is the clearest case

`Aggregator` — the `(init, add, merge, present)` interface every
aggregator here implements — is in `okay`, not here. The reason is a
measurement rather than a preference. When the core was surveyed on
2026-09-18:

| | users among the 73 modules |
|---|---|
| `Aggregator`, the interface | 10 |
| `Sketch`, an implementation of it | 0 |

An interface with ten users beside an implementation with none, in
the same subject, in the same core. That is the rule the whole
modularisation runs on, stated by the code itself: **an interface
stays, the machinery that implements it leaves.** See
[specs/core-modules.md](../../specs/core-modules.md).

## Using it

The package is still `okay`, so no import changes. Declare the module:

```scala
lazy val myModule = (project in file("my-module"))
  .dependsOn(okay.jvm, okayData.jvm)
```

Two modules in this repository needed that line, okay-crdt and
okay-security.
