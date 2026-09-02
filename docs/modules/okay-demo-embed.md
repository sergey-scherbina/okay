# okay-demo-embed

> A live proof, not a library: the chat demo's attribute registry
> wired to a real embedder (specs/demo-chat.md, demo-embeddings-attr)
> — `okay-langchain4j-embed`'s `Langchain4jEmbed.embed(model)` plugged
> into `ChatDemo.marketOf`'s `embed` parameter.

Depends on: `okay-demo`, `okay-langchain4j-embed` (a real ~90MB ONNX
model download — deliberately kept OUT of the root `.aggregate(...)`
list and not a dependency of `okay-demo`'s own test sourceset). Build
and test it explicitly: `sbt okayDemoEmbed/test`.

## What it proves

`MatchStore.propose`'s search-before-create compares descriptions by
cosine similarity; `Vectors.hashing()` (the demo's zero-dependency
default) is lexical, so two synonymous descriptions sharing no
substring never collide and the registry drifts into near-duplicate
attributes. `TestDemoEmbed` proposes "разработчик" then "программист"
through `ChatDemo.marketOf(":memory:")` (the demo's own defaults) and
gets TWO attributes — the drift, demonstrated, not asserted from a
distance — then the same two proposals through
`marketOf(":memory:", embed = Langchain4jEmbed.embed(model),
proposeThreshold = 0.5f)` collide into ONE.

The threshold is measured against the actual model, not invented:
this ONNX MiniLM scores that word pair at ~0.52 cosine, well under
`MemoryMatch`'s conservative 0.85 default (chosen to avoid false
merges on the coarse hashing fallback) — wiring in a real embedder is
a package deal with recalibrating the threshold to its own similarity
distribution, which is why `marketOf` takes both parameters together.
