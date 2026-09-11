# okay-onnx

> The direct ONNX session (specs/intent-spans.md): the same model file
> `okay-langchain4j-embed` wraps, opened by this repository's own hands
> so that the TOKEN vectors come back beside the pooled one — one
> forward pass, two readings. Not a second encoder.

Depends on: `okay-rag`, the ONNX runtime and the HuggingFace
tokenizers (native libraries), and a model directory on disk
(`model.onnx` + `tokenizer.json`, as sentence-transformers exports
ship — ~120MB). Deliberately kept OUT of the root `.aggregate(...)`
list in build.sbt, as `okay-langchain4j-embed` is; build/test it
explicitly with `OKAY_ONNX_MODEL=<dir> sbt okayOnnx/test`, and without
the variable the suite SKIPS and says so.

## Guide

**One pass, two readings.** `Encoder(modelDir).encode(text)` answers
`Encoded(tokens, pooled)`: the vector for every token WITH its
character offsets (`okay.rag.Token`), and the pooled sentence vector
— mean over every position, then unit length, which is what the
langchain4j wrapper computes, so a consumer that switches sees the
vectors it had (measured at cosine 1.0000 by okay-chat).

**As what the tiers take.** `encoder.embed: String => Embedding` is
the plain function okay-intent's classifiers name as their
dependency; `encoder.tokens: Tokens` is the seam `okay.intent.Spans`
reads through; `encoder.handler: Handler[Embed]` is okay-rag's
effect, exactly as `Langchain4jEmbed.handler` offers it.

**What the token vectors are for.** The slot layer — which words of a
message are the place, the time, the thing — from vectors the
sentence encoder computes anyway and mean pooling throws away. The
tier that reads them is pure and lives in okay-intent (`Spans`); this
module is only the door that keeps them. The measurement that
justified the door is okay-chat's specs/meaning.md: 317 live turns,
the place at 93% agreement with a hand-written city table and five
finds beyond it, with nothing fitted.

**The one cast.** The runtime hands back `Object`; the model's own
signature says `float32[batch, sequence, dim]`, and it is checked
once at open, so the cast in `hidden` is the type the model declared
and a model of another shape fails at construction, not per message.
