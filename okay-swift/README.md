# okay-swift — the iOS client that never changes

A Swift client of an okay frontend (specs/frontend.md "Mobile", M2).
As dumb as a browser: it draws what the server sends and reports what
the user did; every piece of application logic lives on the server.
It depends on NOTHING of okay — it implements
`../docs/protocol/frontend.md`, and its proof is
`../docs/protocol/conformance.jsonl`, replayed by its test.

```
cd okay-swift
swift test                                                    # the conformance proof
swift run okay-smoke "ws://127.0.0.1:8080/counter?__live=counter" inc   # headless: connect, press, patch
xcodebuild -scheme OkayUI -destination 'generic/platform=iOS Simulator' build
```

## Layout

| | |
|---|---|
| `Sources/OkayProtocol` | the shapes (`Model.swift`, the document transcribed), the codec (`Wire.swift`: a sum is `{"Case": {...}}`, reading is total), the tree (`Tree.swift`: patch application, the hybrid rule — a Form's fields fold here, its button submits once) |
| `Sources/OkayUI` | `Client.swift` (`URLSessionWebSocketTask`, hello first, events queued until the socket opens), `Render.swift` (level L in SwiftUI; `OkayApp(url:)` is the whole client as a view) |
| `Sources/okay-smoke` | the headless check (macOS) |

## In an app

An iOS app is an Xcode project that adds this package and shows
`OkayApp(url: URL(string: "wss://your.server/page?__live=id")!)`.
That is the entire app: the server changes everything else.

## What it claims

Nothing: `Hello {vocab: []}`. Every semantic node is lowered by the
server before it is sent; `Render.swift` still draws the semantic
level in case a future version claims some of it.
