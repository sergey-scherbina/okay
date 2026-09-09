# Typed spans from token vectors, and the encoder that keeps them

## Overview

A sentence encoder computes a vector for every token and mean pooling
throws them away to make one vector for the sentence. The intent
tiers of this repository (specs/intent-classify.md) work on that one
vector: what KIND of thing was said. This spec is about the vectors
that were thrown away, and the question they can answer that a
sentence vector cannot: WHICH WORDS of the message are the place, the
time, the money, the thing wanted — the slot layer, which every
consumer so far has written as regular expressions.

The consumer that asked measured first (okay-chat, `specs/meaning.md`,
317 live turns, one evening, 2026-09-09). With nothing fitted — a few
dozen authored phrases per slot, scored by cosine against every window
of words — the token vectors matched the hand-written city table on
the place 93% of the time and found five places it could not, matched
the time extractor 86% and found «на этой неделе» ×4 and «на
выходных», which no parser there could name; and were worst at the two
slots that turned out not to want vectors at all: money is a SHAPE
(`Amount`), and the thing wanted is a REMAINDER — what is left of the
sentence when the typed spans are out.

Two lessons came out of that measurement and both are built in here:

1. **A prototype embedded ALONE is the wrong reference for a span
   INSIDE a sentence.** The encoder is contextual: «во Вроцлаве» in a
   question scored 0.45–0.59 against standalone phrases, and a bare
   «Вроцлав» 0.81 — by 0.2–0.3 of cosine the two conditions differ.
   So a prototype is embedded in a CARRIER sentence of its language
   («нужен сантехник {}») and only its own tokens are pooled.
2. **A preposition absorbs its phrase.** With every window a
   candidate, «в», «на», «по», «з» won whole slots on their own. A
   span may open on a function word and may not close on one, nor be
   made of them.

## Interface

    okay.rag.Token(text, start, end, vector)         a vector with a location
    okay.rag.Tokens = String => Vector[Token]        an encoder that keeps them

    okay.intent.Spans
      inContext(tokens: Tokens, carrier, phrase)     the phrase's own token vectors, from inside the carrier
      train(protos: Seq[(slot, Vector[Embedding])])  Trained: the phrase vectors per slot and a centroid each
      windows(text, most, function)                  candidate spans: word windows, the function-word rule
      find(tokens, text, model, threshold, most)     the best window per slot at or above the threshold
      Span(slot, text, start, end, score, nearest)   what it found, and how sure by two measures

    okay.intent.Fitted.SpansModel                    the model as data, save/load like every tier

    okay.onnx.Encoder(modelDir)                      the direct session: model.onnx + tokenizer.json
      encode(text): Encoded(tokens, pooled)          ONE forward pass, both readings of it
      embed: String => Embedding                     the pooled vector — the production embedding
      tokens: Tokens                                 the token vectors with their characters
      handler: Handler[Embed]                        okay-rag's effect, as Langchain4jEmbed offers it

`Spans` is pure — vectors in, spans out — and crosses to JS. The
encoder is JVM only and lives in its own module, `okay-onnx`,
deliberately outside the root aggregate as `okay-langchain4j-embed`
is: a native runtime and a model on disk are not things every
contributor's `sbt test` should need.

## Behavior

- [x] `inContext` returns exactly the tokens overlapping the phrase's
      characters inside the carrier, and none of the carrier's own
- [x] `train` normalises each phrase vector and each centroid; a slot
      with no phrases is absent, not a zero vector
- [x] `windows` never yields a window that ends on a function word or
      is made only of them, and does yield one that opens on one
- [x] `find` answers the best window per slot, only at or above the
      threshold, with the centroid score and the nearest-phrase score
      both reported
- [x] a `SpansModel` survives `Fitted` save and load byte for byte
- [x] `Encoder.encode`'s pooled vector equals the mean of its token
      vectors over every attended position — the production
      embedding, not a second one (okay-chat measured cosine 1.0000
      against the langchain4j pooling of the same file)
- [x] `Encoder` tokens carry character offsets that cover the text and
      drop the two specials; the suite SKIPS, and says so, when no
      model directory is named

## Design

**The tier takes embedded prototypes, not phrases.** `Centroid.train`
takes embeddings and knows nothing of the encoder; so does `Spans`.
`inContext` is the one place the lesson about context lives, and it
takes the encoder as a function, so a test can hand it a fake.

**Two scores, both reported.** The centroid is the cheap score and
the one the threshold is set on; the nearest phrase is what a reader
looks at to see WHY a span scored — «Варшава,» ← `у Варшаві` — and
the two disagree exactly where a slot's phrases do not form one
cluster, which is itself worth seeing.

**Windows are words, not word-pieces.** The tokenizer splits «Вроцлаве»
into pieces; a span that ends mid-word is not a thing a person said.
Windows are whitespace-delimited words by character offset, and a
window's vector is the mean of every token overlapping it.

**The function-word list is a parameter with a default.** The
default covers the four languages the measurement was made in (ru,
uk, pl, en); a consumer in another language passes its own. It is a
set of surface forms, lowercased and stripped of punctuation —
nothing here knows a language.

**One pass, two readings.** `okay-onnx` does not add an encoder; it
reads the one the consumer already runs one step earlier. The pooled
vector is the mean over every attended token including the two
specials, which is what the langchain4j wrapper computes, so a
consumer switching to `Encoder.embed` sees the same vectors it had —
measured, above. The one cast in the module — the ONNX runtime hands
back `Object` — is isolated in one function that says why the type is
right.

## Decisions

- **Prototypes in context, never alone** — measured, and built into
  `inContext` rather than left as advice.
- **A span is words** — may open on a function word, may not close on
  one.
- **Pure tier, JVM encoder, separate module** — the same split as
  `Centroid` against `okay-langchain4j-embed`, and `okay-onnx` is the
  operator's name for the module.
- **The static table is a second `Tokens` implementation, later** —
  `Static` already holds a vector per token and would give this tier
  a JS road with no runtime; the measurement says context is worth
  0.2–0.3 of cosine, so that road is a compromise to measure, not a
  default. BACKLOG.

## Results — intent-spans (2026-09-09)

Landed as three pieces along the seam the tiers already draw:
`okay.rag.Token` (a vector with a location; not `okay.lex.Span`,
which is a lexer's line-and-column for a document), the pure `Spans`
tier in okay-intent with `Fitted.SpansModel` beside the other four
models, and `okay-onnx`. The tier's tests run over a fake encoder
whose vectors are what a word's spelling says — every property in the
Behavior list is checked without a model on disk. The encoder's tests
run against the real file when `OKAY_ONNX_MODEL` names it, and the
one that matters most is the third: a token inside a sentence agrees
with the same token alone at less than 0.95 — the measurement's
lesson, now a gate, so that «prototypes in context» cannot quietly
become pointless under a future encoder that is not contextual.

Not measured here: the tier over the real encoder on real messages.
That is the consumer's shadow run (okay-chat, `frame-in-shadow`),
which records the frame beside every live turn and answers, over
months rather than one evening, where the threshold sits.
