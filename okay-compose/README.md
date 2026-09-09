# okay-compose — the native client that never changes

A Compose client of an okay frontend (specs/frontend.md, stage 3). It
is as dumb as a browser: it draws what the server sends and reports
what the user did. Every piece of application logic lives on the
server; this program depends on NOTHING of okay — it implements
`../docs/protocol/frontend.md`, and its proof is
`../docs/protocol/conformance.jsonl`, replayed by its test.

```
cd okay-compose
./gradlew :protocol:test                      # the conformance proof
./gradlew :app:run --args "ws://127.0.0.1:8080/counter?__live=counter"
```

The second line connects to an okay-script Live page (`Site.ws`, the
same socket the browser's `live.js` uses); the same server drives the
browser and this window at once.

## Layout

| | |
|---|---|
| `protocol/` | the shapes (`Model.kt`, the document transcribed), the codec (`Wire.kt`: a sum is `{"Case": {...}}`, reading is total), the tree (`Tree.kt`: patch application, the hybrid rule — a Form's fields fold here, its button submits once) |
| `app/` | `Client.kt` (the JDK's WebSocket, hello first), `Render.kt` (level L in Compose Material), `Main.kt` |

## What it claims

Nothing: `Hello {vocab: []}`. Every semantic node is lowered by the
server before it is sent; `Render.kt` still draws the semantic level
in case a future version claims some of it.

## Platforms

Compose Multiplatform's desktop target today. The composables in
`Render.kt` are common code; the Android target is the same source
with an SDK on the build machine and an `androidTarget()` in
`app/build.gradle.kts` — not added until one exists, so that the build
that is checked in is the build that runs.
