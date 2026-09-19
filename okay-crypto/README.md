# okay-crypto

The primitive crypto seam (security-crypto-split): the four operations SCRAM and password hashing need, as a per-platform given that drags NO dependency. It rests on the platform's own crypto — JCA on the JVM, `node:crypto` on JS — and on nothing else, so a module that must not cycle back through the security stack (okay-pg's SCRAM authentication) stands on a shared seam instead of a private copy of the same four functions.

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-crypto.md`](../docs/modules/okay-crypto.md) | what it is, and the reasoning |
| [`specs/tls.md`](../specs/tls.md) | the design and its decisions |
