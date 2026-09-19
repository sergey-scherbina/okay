# okay-demo-e2e-browser

One real chat round through a REAL headless browser (Playwright), over the SAME server every other demo test boots: okay-jetty, the scripted model, a random port. It proves the browser half that a JVM unit test cannot reach — `Main.scala`'s fetch + `ReadableStream` streaming glue, running in an actual JS engine instead of being approximated by string splitting on the server side.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-demo-e2e-browser.md`](../docs/modules/okay-demo-e2e-browser.md) | what it is, and the reasoning |
| [`specs/integration-test-gate.md`](../specs/integration-test-gate.md) | the design and its decisions |
