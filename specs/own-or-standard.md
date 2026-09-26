# own-or-standard: every implementation of ours has a standard one beside it, on choice

## The rule (operator, 2026-09-25)

"На каждой платформе у нас должен быть для всех таких вещей выбор — наше
или уже существующее стандартное (возможно не в единственном числе) —
код меняться не должен с точки зрения нашего API — только импорты и
имплиситы."

Wherever this repository carries an implementation OF ITS OWN of
something the platform or a standard library also provides — written for
no dependency, for the JVM, Scala.js and Native alike, or for speed — the
caller's code never names which one runs. It names a FACADE, and the
choice is an import:

```scala
summon[Compression].zstd.compress(bytes)          // ours, with no import
import okay.compress.Aircompressor.given           // the library's, JVM
```

The shape, fixed by okay-arrow (`ArrowCodec`/`OkayArrow`/`ApacheArrow`)
and now the norm:

1. **A facade trait** with a `name` and the operations, in the CROSS
   source set, so every platform sees one API.
2. **Ours is the default given**, in the facade's companion, on every
   platform: `given okay: Facade = Okay`. A program that imports nothing
   runs on our implementation and depends on nothing.
3. **Each standard implementation is an object behind an import**, in the
   platform's source set (`scala-jvm`, `scala-js`, `scala-native`),
   `object Library extends Facade { given library: Facade = this }`, over
   an OPTIONAL dependency (Maven `<optional>`: `% "optional;test"`) —
   nobody depending on the module gets the library transitively, and a
   program that imports the object adds the dependency itself.
4. **Absence is refused BY NAME**: `Library.missing` names the class not
   on the classpath and the artifact to add, or the default to fall back
   to; the first use throws it. Never a `NoClassDefFoundError` from inside
   an operation.
5. **The formats are the same either way**, and a test proves each reads
   the other's output — the choice is one of speed and dependency, never
   of compatibility.
6. **By name too**: `Libraries.byName("okay" | "…")` (JVM) for a config
   value or a flag, the `WireChoice.named` road; a name outside the set is
   a `Left` naming the choices.
7. **"Possibly several"**: a facade may have more than one standard
   implementation per platform (a pure-Java one and a JNI one, say), each
   its own object and import; the default stays ours.

## What has it today

| facade | ours (the default) | standard, behind an import | platforms |
|---|---|---|---|
| `okay.arrow.ArrowCodec` | `OkayArrow` | `ApacheArrow.given` (Arrow Java 19, optional) | JVM (ours everywhere) |
| `okay.compress.Compression` | `Compression.Okay` (`Lz4Frame`, `Zstd`, `Snappy`) | `Aircompressor.given` (io.airlift:aircompressor 2.0.3, optional) | JVM (ours everywhere) |
| `okay.parquet.ParquetCodec` | `OkayParquet` | `ParquetJava.given` (parquet-hadoop 1.16.0 + hadoop-client-api, optional) | JVM (ours everywhere) |
| `okay.lake.AvroReader` | `OkayAvro` | `ApacheAvro.given` (org.apache.avro:avro 1.12.1, optional) | JVM |
| `okay.crypto.Keccak` | `Keccak.Okay` (`Keccak256`) | `BouncyCastleKeccak.given` (bcprov 1.78.1, optional) | JVM (ours on JVM and JS) |

Consumers made transparent with them: okay-arrow's compressed bodies
(`read` takes the `Compression` in scope), okay-cluster's
`RemoteCompression.Lz4/Zstd` givens and `Remote.listen`, okay-x402-evm's
`Evm.keccak`.

What is NOT on the list, and why: SHA-256, HMAC, PBKDF2 and randomness
are the platform's already (`okay.crypto.Crypto`: JCA on the JVM,
node:crypto on JS), Argon2 is BouncyCastle's, TLS is the JDK's, DEFLATE
and zlib on the wire are `java.util.zip` — by specs/tls.md's rule
("platform primitives, never our own") there is no implementation of
ours to choose against. The day one is written, it arrives with the
seven points above.

## Roads, not built

- a JNI zstd (`com.github.luben:zstd-jni`) as a second JVM
  `Compression` — the real zstd, native; the same import shape.
- Scala.js and Native standard implementations (`node:zlib` has no zstd
  before Node 22; a C `libzstd` binding on Native) when a consumer there
  wants them.
- `Lz4Frame` over the library's blocks is our envelope; a library frame
  writer (lz4-java) would be a third object if anyone needs its streams.

## Decisions

- **The facade is a trait with a default given, not a registry.** A
  registry needs the library object touched before it is found;
  an import is visible where the choice is made and nowhere else.
- **Ours stays the default** even where a library is faster: the default
  must run with no dependency on every platform, which only ours does.
- **`byName` lives on the JVM** (`Compressions`, `Keccaks`): the cross
  companion cannot name JVM-only objects, and picking by a config string
  is a JVM application's concern.
