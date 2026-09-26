# Foreign languages

okay runs code written in Python, R, TypeScript, Go, Rust and Haskell in
their own processes (or, for Rust and Go, inside this one), and code
written in Clojure, Frege and Java inside the JVM. This page is the whole
model in one place. The pages after it are its chapters, one per language
or per layer ([below](#the-chapters)); the design, with every decision and
measurement, is [specs/foreign-one.md](../specs/foreign-one.md).

## What crosses

Five things cross a language boundary, and the SHAPE of what you hand
over decides how it crosses. There is no "fast mode" switch:

| you hand over | it crosses as | e.g. |
|---|---|---|
| a value | one message of the wire: JSON lines, or CBOR when a given asks | `Py.fn[Double]("m:f")(x)` |
| a table | ONE value, column by column: Arrow where both sides speak it, the wire's columnar JSON where they do not, the same object on the JVM | `Road.rows`, `Frames` |
| an object | a handle; the object stays where it was made, and dies when released | `Py.hold`, `R.hold`, `Holds` |
| a stream | chunks, pulled at the consumer's pace: the next is asked for only when the last was taken | `Py.stage`, `Py.source`, `R.source` |
| a program | data: an answer, or one operation and the rest of the program as a function of its answer | `Foreign.program`, `Frege.run`, `Program.run` |

A program is how the other language takes part in okay's effects. Its
operations are run by the SCALA side's handlers, so a Reader, a transaction
or a Choice that makes every branch works the same whether the program was
written in Scala or not.

## One program, every language

The caller declares its operations ONCE, in Scala, as callbacks — here two,
a price lookup and a discount, both reading a Reader:

```scala
val priceOf = Foreign.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
val discount = Foreign.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))
val cbs = Foreign.callbacks(priceOf, discount)
```

The program that uses them, in each language. Python and R perform by
name:

```python
def total(sku, qty):
    return okay.perform("price_of", sku).then(lambda price:
           okay.perform("discount", price * qty))
```

```r
total <- function(sku, qty)
  okay_then(okay_perform("price_of", sku), function(price)
    okay_perform("discount", price * qty))
```

Go, Rust and Haskell perform TYPED operations, generated from the same
callbacks (`Go.ops`, `Rs.ops`, `Hs.ops`), so a wrong argument is their
compiler's error:

```go
func total(sku string, qty int64) okay.Program[float64] {
	return okay.Bind(okay.Send(shop.PriceOf(sku)), func(price float64) okay.Program[float64] {
		return okay.Send(shop.Discount(price * float64(qty)))
	})
}
```

```rust
fn total(sku: String, qty: i64) -> Program<f64> {
    send(ops::price_of(sku)).and_then(move |price| send(ops::discount(price * qty as f64)))
}
```

```haskell
total :: String -> Integer -> Eff '[Shop] Double
total sku qty = do
  price <- send (PriceOf sku)
  send (Discount (price * fromInteger qty))
```

Frege performs them through a module `Jvm.frege` writes, typed the same
way, and Clojure through a namespace from `Jvm.clojure`
([details](jvm-languages.md#the-callers-own-operations-declared-once)):

```haskell
quote :: String -> Long -> Prog Double
quote sku qty = do
  price <- perform (priceOf sku)
  perform (discount (price * qty.double))
```

And the Scala that runs it is the SAME line whichever language answers and
however it is reached — a pipe, TCP, a gateway, FFM or WebAssembly:

```scala
val run = Foreign.program[Double](address("total")).calling(Foreign.callbacks(priceOf, discount))("tea", 3L)
assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(run.program).runWith, Right(6.0))
```

For the JVM languages the program is a value in this process, so the
callbacks go to its driver:

```scala
val quote = Frege.run[Reader % Map[String, Double], java.lang.Double](Quote.teaForThree.call(), calls = Jvm.calls(cbs))
```

Python, R, TypeScript, Go and Rust also have DIRECT style: ordinary code
calling `okay_call("price_of", sku)` and getting the answer back, the
effect still run by the Scala side:

```scala
val quote = Foreign.fn[Double](address("quote")).calling(Foreign.callbacks(priceOf, discount))("tea", 3L)
```

## Who carries what

| | value | table | object held | stream | program, multi-shot | direct `okay_call` |
|---|---|---|---|---|---|---|
| Python | yes | yes (Arrow or columnar) | yes | stages, sources | yes | yes |
| R | yes | yes (Arrow or columnar) | yes | stages, sources | yes | yes |
| TypeScript | yes | yes (columnar) | yes | — | yes | yes |
| Go | yes | yes (columnar) | values (no methods) | — | yes | yes |
| Rust | yes | yes (columnar; Arrow C Data in process; not on wasm) | values (no methods; not on wasm) | — | yes | yes (not on wasm) |
| Haskell | yes | yes (columnar) | values (no methods) | — | yes | — |
| Clojure, Frege | yes | by reference | yes (JVM objects) | stages | yes | — |

A dash is an honest absence and, through the facade, a compile error
rather than a run-time surprise ([foreign-facade](foreign-facade.md)).
Go, Rust and Haskell hold VALUES: a call made `held` keeps its answer in
the worker, a ref passed back is that value again, `release` drops it —
there are no methods to call by name on them, and a held value is read,
not changed, so a stateful stage (which changes its state) stays with the
languages whose objects have state.

## Two runtimes

Everything above runs on one of two things:

- **The wire**, for every language in another process or behind FFM or
  WebAssembly: ONE engine (`WireSession`) under one handler
  (`ForeignWorker`), whatever the language and whatever the link. The
  hello says what the far side speaks (CBOR, DEFLATE, columnar frames,
  Arrow); givens choose among it (`WireFormat`, `WireCompression`,
  `WireAuth`, `WireSecurity`, `WireDeadline`); one supervisor restarts a
  dead worker and replays what it had answered; one `Pool` leases workers
  and routes a held object back to the worker that holds it.
- **The walker**, for Clojure, Frege and Java inside the JVM
  (`okay.Foreign`): the other language's program is read node by node
  and each operation performed in the caller's row. Nothing is copied.

## The wire, in five operations

`call` (a function, a method or an attribute, by address; `held` keeps the
answer on the far side as a handle; a table as the first argument),
`program` (start one; its answer is `done` or a `perform` node naming its
continuation), `continue` (answer a node — more than once, for a
multi-shot handler; a direct `okay_call` node says `once`), `forget` (drop
a run's continuations) and `release` (drop a held object). Around them:
the hello, `configure` and `auth`. The protocol IS a transcript,
[`okay/py/wire.txt`](../okay-py/src/main/resources/okay/py/wire.txt): the
host must send exactly its requests, the reference far side answer exactly
its replies, and every language answers the same conformance suites
(`WireConformance`, `CrashConformance`) over every link.

## Adding a language

1. **A library that speaks the wire**: the hello, the five operations, the
   value tags, the columnar frame. The transcript is the specification.
2. **A row in the conformance suites**: `WireConformance` and
   `CrashConformance` take the far side's `Shape` (its value rules) and
   its link; green on both is what "supported" means.
3. **Typed operations**, when the language has types: a generator from the
   Scala callbacks, as `Hs.ops`, `Go.ops`, `Rs.ops`, `Ts.ops` and
   `Jvm.frege` are.
4. **A `Language` value** for the facade and the cluster
   (okay-foreign-cluster): how to open its pool, how it addresses a
   function, its `Shape`. One small object; nothing else is per language —
   `Language.ts` and `Language.worker` (any compiled worker) are twelve
   lines each.

## The chapters

- [okay with Python and R](python-and-r.md) — typed calls, callbacks,
  held models, stages and sources, Arrow frames, environments, a journal.
- [Rust and Go as okay, one language](one-language.md) — every link, the
  wire's encoding, auth, TLS, the gateway, recovery, and which language
  supports which layer.
- [okay with Go](go.md), [okay with Rust](rust.md),
  [okay with TypeScript](typescript.md) — each language's own page.
- [One facade over every foreign language](foreign-facade.md) — the
  capability typeclasses: a job that compiles against exactly what it
  uses.
- [okay with other languages](jvm-languages.md) — Java streams, Clojure
  and Frege inside the JVM.

## Literature

- Matthias Grimmer, Chris Seaton, Roland Schatz, Thomas Würthinger, Hanspeter Mössenböck. *[High-performance cross-language interoperability in a multi-language runtime.](https://doi.org/10.1145/2816707.2816714)* DLS 2015. Truffle's interop protocol: one set of messages every language answers — here the five operations, over a wire as well as in process.
- Philip Wadler, Stephen Blott. *[How to make ad-hoc polymorphism less ad hoc.](https://doi.org/10.1145/75277.75283)* POPL 1989. A capability as a typeclass and its absence as a compile error: the "who carries what" table, checked.
- Gordon Plotkin, Matija Pretnar. *[Handling algebraic effects.](https://doi.org/10.2168/LMCS-9(4:23)2013)* LMCS 2013. A program as operations and continuations, run by a handler that may resume more than once — what a foreign program is here, with the handler on the Scala side.
- Joe Armstrong. *[Making reliable distributed systems in the presence of software errors.](https://erlang.org/download/armstrong_thesis_2003.pdf)* PhD thesis, 2003. Ports: a foreign program as a process that speaks a protocol, supervised and restarted — the wire and its supervisor.
- Project Jupyter. *[Messaging in Jupyter.](https://jupyter-client.readthedocs.io/en/stable/messaging.html)* A kernel says what it speaks before anything runs: the hello.
- Mark Raasveldt, Hannes Mühleisen. *[Don't hold my data hostage: a case for client protocol redesign.](https://doi.org/10.14778/3115404.3115408)* PVLDB 2017. Why a table crosses as columns and not as cells.
- Apache Arrow. *[The Arrow C Data Interface.](https://arrow.apache.org/docs/format/CDataInterface.html)* and *[Arrow Flight RPC.](https://arrow.apache.org/docs/format/Flight.html)* One memory format on both sides: IPC streams over the wire, and the two C structs a table crosses as into a Rust library in this process ([details](one-language.md#a-table-in-every-language)).
- *[Reactive Streams, §3 (Subscription).](https://github.com/reactive-streams/reactive-streams-jvm/blob/master/README.md#3-subscription-code)* and M. Thomson, C. Benfield (eds.). *[RFC 9113, HTTP/2, §5.2 Flow Control.](https://www.rfc-editor.org/rfc/rfc9113#section-5.2)* Demand signalled by the consumer: a source here is pulled one chunk per request, and credits for a far side running ahead wait for their first caller (backlog foreign-mux-duplex).
