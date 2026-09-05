# okay-demo-e2e-browser

One real chat round through a REAL headless browser (Playwright),
over the SAME server every other demo test boots: okay-jetty, the
scripted model, a random port. It proves the browser half that a JVM
unit test cannot reach — `Main.scala`'s fetch + `ReadableStream`
streaming glue, running in an actual JS engine instead of being
approximated by string splitting on the server side.

| | |
|---|---|
| `TestChatBrowser` | the module's whole content: open the page, send a turn, assert the streamed answer appears in the DOM |

Kept in its own module and DELIBERATELY out of the root
`.aggregate(...)`, the same reasoning as
[`okay-langchain4j-embed`](okay-langchain4j-embed.md) and
a heavyweight optional module: Playwright's browser
download is a real cost this suite alone should pay. It is also
`Live`-tagged, so `sbt integrationTest` runs it and the default gate
does not (specs/integration-test-gate.md).

Run it explicitly, and link the JS first:

```
sbt "okayChatWebJS/fastLinkJS" "okayDemoE2eBrowser/test"
```
