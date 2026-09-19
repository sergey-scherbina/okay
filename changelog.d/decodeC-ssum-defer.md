## decodeC-ssum-defer - SSum's own recursion deferred like every other case

`Json.decodeC` and `Cbor.getC`'s `SSum` branch called its own
recursion directly (`decodeC(sc(), v).map(...)` /
`getC(in, sc()).map(...)`) instead of through `Cont.defer`, the only
recursive case in either trampoline that didn't. Found investigating
`stackbytes-json-read-not-flat-on-aarch64` (`okay-codec/BUGS.md`) —
it does not explain that entry (`Tree` there has no sum type), but is
real.

`Schema.derived` always wraps a case's payload in a product, and a
product's own recursive field IS deferred, so a derived schema can
never observe this: checked directly, a `derives`-built recursive
`enum` at 500 levels passes on a 256 KB stack whether or not the fix
is applied. The gap is reachable only through a hand-built
`Schema.SSum` whose case recurses with no product between, which
`SSum`'s own type (`cases: Vector[(String, () => Schema[? <: A])]`)
allows and nothing stops. `TestDecodeSumDefer` builds exactly that (a
`Wrap` case whose schema is the enclosing sum itself) and, checked
FAIL first, overflows a 256 KB stack at 500 levels without the fix,
on both wires, and passes with it.

`okayCodecJVM` (217), `okayStaging` (14), `okayCodecJS` (213),
`okayCodecNative` (213, 2 skipped) all green — no behavior change for
any schema `Schema.derived` can produce.
