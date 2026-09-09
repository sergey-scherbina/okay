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
| `app/` | Kotlin Multiplatform: `commonMain` (`Render.kt` — level L in Compose Material; `Client.kt` — the session, hello first, events queued until the socket opens; `App.kt` — `OkayApp(url)`), `desktopMain` (`Main.kt`, `Smoke.kt`, the JDK WebSocket), `androidMain` (`MainActivity`, OkHttp's WebSocket) |

## What it claims

Nothing: `Hello {vocab: []}`. Every semantic node is lowered by the
server before it is sent; `Render.kt` still draws the semantic level
in case a future version claims some of it.

## Platforms

Desktop (JVM) and Android, from ONE set of composables; only the
socket is per platform. Android needs an SDK on the build machine:

```
brew install --cask android-commandlinetools
sdkmanager --sdk_root=$HOME/Library/Android/sdk "platform-tools" "platforms;android-35" "build-tools;35.0.0"
ANDROID_HOME=$HOME/Library/Android/sdk ./gradlew :app:assembleDebug   # app/build/outputs/apk/debug/app-debug.apk
```

Install the APK on a device or emulator and launch it with the
server's address (`10.0.2.2` is the emulator's name for the host):

```
adb install app/build/outputs/apk/debug/app-debug.apk
adb shell am start -n okay.compose.app/.MainActivity --es url "ws://10.0.2.2:8080/counter?__live=counter"
```
