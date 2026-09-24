# Rust and Go as okay

okay's effects are Scala's. This page is about Rust and Go code that
takes part in them as if it were written in the same language. It is
called with types, it calls okay's effects in the middle of its work, and
okay may resume it more than once. How it is reached is a choice made at
the edge, not something the program has to know:

| how Scala reaches it | Rust | Go |
|---|---|---|
| a child process, over its pipes | ✅ | ✅ |
| TCP, another process or another machine | ✅ | ✅ |
| in this process, native code through FFM | ✅ | — a Go runtime does not belong in the JVM ([why](rust.md#go)) |
| in this process, WebAssembly under Chicory | ✅ programs as data (no threads in wasip1, so no direct style) | ✅ |

## One program, two languages

The far side serves `quote(sku, qty)`. It is ordinary code that calls two
of okay's effects (a price, a discount) and gets their answers: the
direct style, `okay_call(request) -> answer`.

In Rust:

```rust
    functions.insert("quote".into(), function(|ctx, args| {
        let sku = String::from_value(&args[0])?;
        let qty = i64::from_value(&args[1])?;
        let price = ctx.call_op(ops::price_of(sku)).map_err(|e| e.to_string())?;
        let total = ctx.call_op(ops::discount(price * qty as f64)).map_err(|e| e.to_string())?;
        Ok(total.to_value())
    }));
```

In Go:

```go
func quote(c *okay.Ctx, args []any) any {
	price, err := okay.CallOp(c, shop.PriceOf(args[0].(string)))
	if err != nil {
		panic(err)
	}
	total, err := okay.CallOp(c, shop.Discount(price*float64(args[1].(int64))))
	if err != nil {
		panic(err)
	}
	return total
}
```

`ops::price_of` and `shop.PriceOf` are not written by hand. `Rs.ops` and
`Go.ops` generate them from the Scala callbacks that answer them, so an
argument of the wrong type does not compile, and `price` has the type of
the callback's answer. The effects are declared ONCE, in Scala:

```scala
  val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
```

## The same Scala, over every link

The engine is `ForeignWorker`, and only its link changes:

```scala
  lazy val engine: ForeignWorker = ForeignWorker.speaking(Seq(GoWorkerBinary.binary.toString))
```

```scala
      case Some(n) => (ForeignWorker.connect("127.0.0.1", n), p)
```

```scala
    ForeignWorker.over(InProcessLinks.ffm(NativeLib.load(RustInProcess.dylib)).fold(why => throw IllegalStateException(why), identity))
```

```scala
    ForeignWorker.over(InProcessLinks.wasm(WasmLib.load(Files.readAllBytes(GoInProcess.wasm))))
```

The call is then the same everywhere:
`Foreign.fn[Double]("quote").calling(Foreign.callbacks(priceOf, discount))("tea", 3L)`
runs `quote` in Rust or Go. Each `okay_call` is answered by a Scala
callback under the caller's handlers (here, a `Reader` of prices). The
same holds for programs as data (`Foreign.program`), which a `Choice`
handler resumes on every branch, and for `Durable` journals.

## How it is held to that

ONE Scala test body, `WireConformance`, runs over every cell of the table.
It checks four things:
- multi-shot across the link, every branch of two choices;
- each operation answered by a Scala callback under the caller's Reader;
- direct style, `okay_call` twice;
- a failure: a condition by name, with the far side living on. Rust
  compiled to WebAssembly is the exception: its panic traps the module,
  and the test holds that the panic's own message is reported.

A mutant was caught on each link while it was built. One mutant was not
caught at first, and that found a real defect: the engine read wire
lines with a repairing JSON parser, so a reply cut one byte short passed.
Wire lines are now read strictly, on every transport.

## The wire's encoding, chosen by a given

The link carries JSON lines unless the Scala side says otherwise, and
it says so with an import. The imports are givens that the engine's
`start`, `speaking`, `connect` and `over` take as `using` parameters:

```scala
  import WireFormat.Cbor.given
  import WireCompression.Deflate.given
```

With those two lines in scope, the same engine call speaks CBOR
(RFC 8949), each message compressed with raw DEFLATE (RFC 1951):

```scala
  lazy val engine: ForeignWorker = ForeignWorker.start(TestPy.python.get, modules = Seq(PyConformance.conf))
```

Nothing else in the program changes. Without an import, the defaults are
JSON with no compression. They come from the companions' implicit scope,
so an old program keeps its old wire byte for byte.

The far side cannot import a Scala given, so it ANNOUNCES what it speaks
in its hello: `"speaks":{"format":["json","cbor"],"compress":["deflate"]}`.
The engine then sends one `configure` request as a JSON line. The far
side answers it in the old mode and switches after the answer. From then
on every message is a frame: a 4-byte big-endian length, then the bytes.
A choice the far side did not announce is refused by name before any
request is sent, and never quietly downgraded:

```scala
    val e = intercept[IllegalStateException](ForeignWorker.speaking(Seq(HsConformance.binary)))
    assert(e.getMessage.contains("given WireCompression is deflate"), e.getMessage)
```

Each far side uses its own platform's mechanism. None needs a package
the language does not already ship:

| Far side   | CBOR                               | DEFLATE                     | Links tested                    |
|------------|------------------------------------|-----------------------------|---------------------------------|
| Python     | its own subset (struct)            | zlib, `wbits=-15`           | pipes                           |
| TypeScript | its own subset (Buffer)            | `node:zlib` raw             | pipes                           |
| Go         | its own subset                     | `compress/flate`            | pipes, TCP, WebAssembly         |
| Rust       | its own subset over serde_json     | flate2 (pure Rust backend)  | pipes, TCP, FFM, WebAssembly    |
| Haskell    | its own subset (bytestring, text)  | none: GHC ships no zlib     | pipes                           |
| R          | not yet (okay-r's own engine)      | not yet                     | —                               |

The "subset" in each row is the same: integers, floats (half, single,
double), text, definite arrays and maps with text keys, and
true/false/null. The tree is the one JSON carries, escapes included.
So `{"t":"int"}` for an integer past 2^53 means the same thing in both
formats, and the conformance suite (`WireConformance`) runs unchanged
under each: `TestGoPipesCbor`, `TestRustFfmCbor`, `TestPyPipesCbor`,
`TestTsPipesCbor`, `TestHsPipesCbor` and the rest.

Why a given rather than a flag: the choice is fixed where the program is
compiled, and the compiler carries it to every engine the program
builds. A missing or ambiguous choice is a compile error, not a run-time
surprise. This is the implicit calculus's point: the context is resolved
by type, in scope, and passed without being written at each call
(Oliveira et al., "The implicit calculus", PLDI 2012,
doi:10.1145/2254064.2254070; Odersky et al., "Simplicitly: foundations
and applications of implicit function types", POPL 2018,
doi:10.1145/3158130). The formats themselves: C. Bormann and P. Hoffman,
RFC 8949, "Concise Binary Object Representation (CBOR)", 2020,
doi:10.17487/RFC8949; P. Deutsch, RFC 1951, "DEFLATE Compressed Data
Format Specification", 1996, doi:10.17487/RFC1951.

## Why a dialogue, even in-process

`okay_call` in this process could have been a C upcall into the JVM. It
is not, and the reason is okay's, not FFM's. A callback is an okay
PROGRAM that must run under ALL the caller's handlers. An upcall arriving
in the middle of a foreign call would run it under the foreign engine's
handler alone, with its `Reader`, `State` or `Async` missing. So every
transport carries the same `ask`/`resume` dialogue, and in-process each
step is one function call.

## Limits

- **Remote TCP is plain TCP, unauthenticated.** Use it inside a trusted
  network or behind TLS or SSH. Encryption and authorization chosen by
  `given`s are the next stage
  ([specs/polyglot-one-wire.md](../specs/polyglot-one-wire.md)).
- **No read deadline.** A far side that stops answering (or confirms a
  `configure` and does not switch) leaves the engine waiting. The
  gate's stall watchdog is what caught that mutant.
- **Rust on WebAssembly.** No direct style, and a panic ends the module.
- **Go in-process.** Only as WebAssembly.

More on each: [okay with Rust](rust.md), [okay with Go](go.md), and
[where each language names okay's effects](jvm-languages.md#where-each-language-names-okays-effects).
