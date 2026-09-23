# scala2/

okay for **Scala 2.13**: the facade modules, all in package `okay.scala2`,
and the Scala 2.13 probe that proves them from a real 2.13 compiler.
Each module is a Scala 3 project whose API scalac 2.13 reads through
`-Ytasty-reader`; its artifact is `dev.okay:okay-scala2-<name>_3`.

| directory | wraps |
|---|---|
| [`okay-scala2`](okay-scala2) | the core: `Prog`, `Eff` and its capabilities, `Cont`, your own effects, `Source`, fibers, channels, `Choose`; and [`probe/`](okay-scala2/probe), the Scala 2.13 test suites |
| [`okay-scala2-codec`](okay-scala2-codec) | okay-codec: `Schemas`, JSON, CBOR, JSON Schema |
| [`okay-scala2-http`](okay-scala2-http) | okay-http: routes, a server, a client |
| [`okay-scala2-ws`](okay-scala2-ws) | WebSockets |
| [`okay-scala2-sql`](okay-scala2-sql) | okay-sql: `Db`, queries, transactions |
| [`okay-scala2-agent`](okay-scala2-agent) | okay-agent: `Model`, `Tools`, `Chat`, durable agents |
| [`okay-scala2-ui`](okay-scala2-ui) | okay-ui: views, forms, dialogs |
| [`okay-scala2-resilience`](okay-scala2-resilience) | okay-resilience: `Guards` |
| [`okay-scala2-persist`](okay-scala2-persist) | okay-persist: `Persist` |
| [`okay-scala2-stm`](okay-scala2-stm) | okay-stm: `Tx`, `Stm` |
| [`okay-scala2-stores`](okay-scala2-stores) | okay-cache, -blob, -docs: `Caches`, `Blobs`, `Documents` |
| [`okay-scala2-llm`](okay-scala2-llm), [`-rag`](okay-scala2-rag), [`-mcp`](okay-scala2-mcp) | token streams, a vector index, MCP client and server |
| [`okay-scala2-optics`](okay-scala2-optics) | okay-optics: `Lens`, `Prism`, `Affine`, `Traversal`, `Iso` |
| [`okay-scala2-workflow`](okay-scala2-workflow) | okay-workflow: durable programs over a journal |
| [`okay-scala2-services`](okay-scala2-services) | okay-actor, -outbox, -obs, -ops, -kafka, -pg |

The guide is [docs/scala2.md](../docs/scala2.md), the reference
[docs/modules/okay-scala2.md](../docs/modules/okay-scala2.md), and the
design [specs/scala2-facade.md](../specs/scala2-facade.md).
