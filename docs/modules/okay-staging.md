# okay-staging

Run-time staging: the staged codec for a `Schema` that exists only at
run time. JVM only, its own module, optional by construction.

## What it is

okay-codec folds a schema in two modes. `Json.encode/decode` interpret
the `Schema` GADT per value; `Staged.json[A]` folds the TYPE's shape at
compile time and emits straight-line code, several times faster. That
macro needs a type. A schema read from a Postgres catalog, declared by
an MCP server, or handed over by R or Python is a VALUE, and until this
module only the interpreter could serve it.

`RuntimeStaged.json(schema)` is the same generator over the value: it
walks the schema's nodes once, generates the straight-line encoder and
decoder for that shape with `scala.quoted.staging`, compiles them in
the running process, and caches the codec by the schema's identity.
The generated code reaches the value's own functions (`make`, `parts`,
`caseOf`, an iso's `to`/`from`, the defaults) through a table of the
schema's nodes handed to it once.

```scala
import okay.codec.{Json, Schema}
import okay.staging.RuntimeStaged

// a composite the catalog described: a product over Seq[Any], no case class
val point: Schema[Seq[Any]] = Schema.SProduct[Seq[Any]]("point",
  Vector("x" -> (() => Schema.SDouble), "y" -> (() => Schema.SDouble)),
  parts => parts, a => a)

val codec = RuntimeStaged.json(point)      // generated once, cached by identity
codec.encode(Seq(1.5, 2.5))                // {"x":1.5,"y":2.5}
codec.decode(Json.parse("""{"x":1}"""))    // Left(missing field 'y' in point) — the fold's own words
```

The generated codec agrees with the interpreter: encode byte for byte,
decode Left for Left with the fold's refusal words on every cold path,
over products, sums, Option/List/Vector, defaults, isos, enumerations,
recursion (which delegates to the fold, as the compile-time generator
does). `TestRuntimeStaged` holds all of it.

## Optional, three ways

1. **A module nobody depends on.** Add `okay-staging` to a program and
   it brings `scala3-staging` and the Scala 3 compiler jar. Leave it
   out and no okay module misses it: every door in okay-codec,
   okay-sql, okay-mcp and the rest is the interpreter or the
   compile-time macro.
2. **A launch switch.** `-Dokay.staging=off` (or `OKAY_STAGING=off`)
   makes every `RuntimeStaged.json` answer the interpreter, and no
   `Compiler` is ever created. `RuntimeStaged.force(Some(false))` does
   the same from code, `force(None)` returns to the switch.
3. **Never a throw.** A generation that fails (a shape the generator
   does not know, a compiler that cannot run in this process) answers
   the interpreter and records why in `RuntimeStaged.lastFailure`.

## What it costs

- The compiler in the process: `scala3-staging` plus the compiler jar,
  tens of MB on the classpath and a heap for it. A process that
  already carries the compiler (okay-script's containers do) pays only
  the staging jar.
- One compilation per schema at first use. The benchmark's
  `generateRuntimeStaged` lane is that number; a schema that lives for
  one call never earns it back.
- Casts. A run-time schema is erased, so the generated code bridges
  `Any` to each node's type in exactly one place, `RuntimeStaged.Unsafe`,
  every cast licensed by the node kind the generator read when it
  emitted the call. Nothing outside that object casts.
- JVM only. The module does not cross to JS or Native, which is why it
  is not part of okay-codec.

## Reaching every generic door: the seam

`RuntimeStaged.install()` makes this generator the provider behind
`okay.codec.Codecs`, and every generic door in okay that takes a
`Schema` as a value goes through that seam: okay-script's session
attributes and Live state, okay-ui's persisted sessions and forms,
okay-persist's typed topics, snapshots, configs and wire frames,
okay-http's JSON bodies, the cluster's frames, a tool's arguments,
the LLM protocols, Redis and Mongo values. One call at boot, and all
of them answer the staged codec; `-Dokay.staging=off` and they all
answer the interpreter. okay-script's `Serve` makes that call and
prints which way it went. A JVM program that cannot depend on this
module calls `okay.codec.Staging.autoInstall()`, which finds it by
name.

`RuntimeStaged.cbor(schema)` is the CBOR twin of `json`, and
`RuntimeStaged.strict(schema)` the strict-JSON one (characters
straight into the schema, no tree) — the same generator with another
emitter, each held to the same agreement suite against its
interpreted door. `Codecs.readStrict[A](text)` is the seam's strict
door; a provider that does not implement it answers the interpreted
`JsonStrict.read`.

## Where it earns its keep, and where it does not

See specs/codecs.md, "Run-time staging": the condition is a schema
that is a value, lives long, and whose fold is a measured share of a
hot path. A tool's arguments decoded a few times a second do not
qualify; a catalog-described row shape decoded a million times does.

## Hygiene: generated code cannot outlive its binder

A generator with an effect can go wrong in one classic way. Say it
keeps a bound variable's code in a cell (a `var`, an okay `State`)
while it is inside that variable's `let` or lambda, and splices it
after the binder has closed. The result is ill-scoped code. Kameyama,
Kiselyov and Sunada call this *scope extrusion*, and it is the problem
their combinators for impure code generation exist to rule out.
MetaOCaml detects it when the code runs.

On this road the check is Scala 3's own, and it comes earlier: staging
refuses the extruded code by name while it is being built ("a
reference to parameter x was used outside the scope where it was
defined"), before anything is compiled or run. `TestScopeExtrusion`
pins that refusal for a host `var` cell and for an okay `State` cell
whose answer carries the code out. Beside them is the control: the
same cell, spliced inside its binder, generates and runs.

`RuntimeStaged`'s own generator gives it nothing to catch. It is a
pure recursion over the schema, and its caches hold compiled codecs,
never `Expr`s. If a generation ever did fail, the door would answer
the interpreter and record why in `lastFailure`. It would not ship
broken code. The other staging road, inline stagers (`Direct.staged`),
cannot extrude at all. The inliner leaves no code value that a cell
could hold. By the time a cell sees a binder, it is an ordinary run-time
value.

> Y. Kameyama, O. Kiselyov, C. Shan, *Shifting the stage: staging with
> delimited control*, PEPM 2009 / JFP 21(6), 2011,
> [doi:10.1017/S0956796811000256](https://doi.org/10.1017/S0956796811000256)
> — let-insertion by delimited control, and why effects in a generator
> threaten hygiene.
>
> Y. Kameyama, O. Kiselyov, C. Shan, *Combinators for impure yet
> hygienic code generation*, PEPM 2014 / Science of Computer
> Programming 112, 2015,
> [doi:10.1016/j.scico.2015.08.007](https://doi.org/10.1016/j.scico.2015.08.007).
