# okay-tls

TLS for the own wires (specs/tls.md): one seam at the TRANSPORT, not per protocol — every wire above gets TLS from it and adds nothing of its own. The vocabulary is postgres's `sslmode`, adopted stack-wide because operators already know it and it names the weak modes honestly; `verify-full` is the only default.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-tls.md`](../docs/modules/okay-tls.md) | what it is, and the reasoning |
| [`specs/tls.md`](../specs/tls.md) | the design and its decisions |
