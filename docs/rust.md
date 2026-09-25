# okay with Rust (and a word on Go)

Rust brings okay COMPUTE: kernels such as hashing, parsing, compression
and SIMD, written once and fast. It does not host okay programs. A Rust
`async` future is not a continuation that okay could resume from
outside, so the effect system stays in Scala, and Rust does the
arithmetic.

A Rust kernel reaches okay as an **effect**, whose operations are the
kernel's calls. A program asks for the operation, and a handler decides
what computes it: the Rust kernel, a JVM implementation, or a test's
stand-in. The program does not change, and the kernel can be swapped per
platform, mocked, and measured.

<!-- not-a-test: a diagram -->
```mermaid
flowchart LR
  P["an okay program<br/>Either[String, Array[Byte]] ! PasswordHash"]
  H1["PasswordHash.rust(lib)<br/>FFM downcall"]
  H2["PasswordHash.using(f)<br/>any function: BouncyCastle, a test"]
  R["libokay_argon2<br/>extern &quot;C&quot; fn okay_argon2id"]
  P --> H1 --> R
  P --> H2
```

## Rust code that performs okay's effects

A kernel is one call: data in, an answer out. Rust code that needs okay's
effects IN THE MIDDLE of its work (read the caller's configuration, ask a
Scala callback, be resumed by a `Choice` handler) is a WORKER instead, like
the Go and Haskell workers. The jar ships a Rust crate, `okay` (its only
dependency is serde_json), and `RustWorker.build(dir)` compiles a crate
against it offline. There are two styles.

**Direct style**, the everyday one: ordinary Rust calls an effect and gets
the answer, `okay_call(request) -> answer`:

```rust
    functions.insert("quote".into(), function(|args| {
        let sku = String::from_value(&args[0])?;
        let qty = i64::from_value(&args[1])?;
        let price = okay_call(ops::price_of(sku))?;
        let total = okay_call(ops::discount(price * qty as f64))?;
        Ok(total.to_value())
    }));
```

**Programs as data**, for multi-shot. A continuation is an `Rc<dyn Fn>`,
so okay can resume it twice, and `Choice` makes every branch:

```rust
fn total(sku: String, qty: i64) -> Program<f64> {
    send(ops::price_of(sku)).and_then(move |price| send(ops::discount(price * qty as f64)))
}
```

- **Typed operations.** `ops::price_of` and `ops::discount` are generated
  from the Scala callbacks by `Rs.ops(Foreign.callbacks(priceOf, discount))`,
  so `price` is an `f64` because the callback answers a `Double`, and a
  wrong argument type does not compile.
- **Serving.** `okay::main(make)` serves on stdin/stdout, or on TCP when
  `OKAY_LISTEN` is set: one binary, either transport. Scala reaches it with
  `ForeignWorker.speaking` or `ForeignWorker.connect(host, port)`, and calls
  it with `Foreign.fn` (direct style) or `Foreign.program`.
- **Panics.** A panic is a `RustError` condition carrying its message, and
  the worker lives on.
- **Checked.** The same Scala test body that checks the Go worker
  (`WireConformance`: multi-shot, callbacks under the caller's Reader,
  direct style, failures) passes over Rust pipes and Rust TCP.

### The same worker, in this process

The same crate also runs with no second process at all. Replace `fn main`
with one line:

```rust
okay::export_worker!(make);
```

Build it as a library (`RustWorker.buildLibrary(dir)` for a `cdylib`, or
`buildLibrary(dir, Some("wasm32-wasip1"))` for WebAssembly). It exports
`okay_exchange(request) -> answer`: one wire line in, one out. The engine
then runs over it as over a pipe:
- **FFM:** `ForeignWorker.inProcess(dylib)` (after `import okay.rust.*`);
- **WebAssembly:** `ForeignWorker.inProcessWasm(module)`, under Chicory.

`okay_call` in-process is the same `program`/`perform`/`continue` dialogue, one
`okay_exchange` per step. It is not a C upcall into the JVM: a callback is
an okay program that must run under ALL the caller's handlers, and an
upcall in the middle of an FFM call would run it under the foreign
engine's handler alone.

The conformance suite passes in-process:
- **FFM:** the full suite, including direct style and a panic the
  worker survives.
- **WebAssembly:** multi-shot and callbacks. `wasm32-wasip1` has no
  threads, so direct style is unavailable there. A panic is `abort` and
  traps the module, and the engine reports it with the panic's message
  ("rust says no") rather than silently.

## The first kernel: Argon2id

`okay-rust/kernels/argon2` is a Cargo crate built as a `cdylib` and a
`staticlib`. It has one dependency, the RustCrypto `argon2` crate, and
`Cargo.lock` is checked in, so it builds offline and reproducibly. It
exports one C function:

```rust
#[no_mangle]
pub extern "C" fn okay_argon2id(
    password: *const u8, password_len: usize,
    salt: *const u8, salt_len: usize,
    memory_kib: u32, iterations: u32, parallelism: u32,
    out: *mut u8, out_len: usize,
) -> i32 {
```

The C ABI is kept small on purpose:
- only pointers, lengths and integers cross it;
- the answer is a code: 0 for done, and negative numbers the Scala side
  names;
- **the caller owns every buffer**, including the output, so nothing is
  allocated in Rust and freed in Java. That means no pair of allocators
  to keep matched, and no leak on an early return.

## From Scala: FFM, then an effect

`NativeLib.load(path)` binds the library through FFM, the JDK's foreign
function API (JEP 454, final in JDK 22). There is no JNI glue and no
generated code. `PasswordHash` is the effect:

```scala
enum PasswordHash[+A] derives okay.Effect:
  case Argon2id(password: Array[Byte], salt: Array[Byte], memoryKb: Int, iterations: Int,
                parallelism: Int, length: Int) extends PasswordHash[Either[String, Array[Byte]]]
```

A program is written against the effect and knows nothing of Rust:

```scala
  def stored(password: String, salt: String): Either[String, String] ! PasswordHash =
    PasswordHash.argon2id(password.getBytes("UTF-8"), salt.getBytes("UTF-8"), memoryKb = 64, iterations = 2, parallelism = 1)
      .map(_.map(hex))
```

There are two handlers:
- `PasswordHash.rust(lib)` calls the kernel. Each call allocates its buffers in a
  confined arena, which is freed when the call returns.
- `PasswordHash.using(f)` answers with any function, for example okay-security's
  BouncyCastle Argon2 or a test's stand-in.

One program, either handler, the same answer:

```scala
    assertEquals(stored("pw", "saltsalt").runWith(using rust), stored("pw", "saltsalt").runWith(using PasswordHash.using(bouncy)))
```

## How it is held honest

- **The law.** For the same inputs, the Rust kernel's bytes are
  BouncyCastle's bytes. The test covers 48 cases: four parameter sets,
  two salts, an empty and a long password, and three lengths. A mutant
  kernel on Argon2 version 0x10 instead of 0x13 fails it.
- **Refusals are values.** Parameters Argon2 refuses come back as
  `Left("okay_argon2id answered -2: parameters Argon2 refuses")`, not an
  exception. A symbol the library does not export is refused by name when
  it is bound, not at the first call.
- **Native access is declared.** The tests fork with
  `--enable-native-access=ALL-UNNAMED`, which JDK 24+ otherwise warns
  about at the first restricted call. An application does the same, or
  names its module.

The module's floor is JDK 22, because FFM is final there (`jdkFloor(22)`,
[JDK compatibility](../specs/jdk-compatibility.md)). The check needs
`cargo`, so it is tagged `Live`, and the effect's own tests run in the
default gate.

## The same kernel as WebAssembly, under Chicory

The same crate compiles to `wasm32-wasip1`: a 70 KB module run by Chicory,
a WebAssembly runtime written in Java. There is no native code in the
process. The kernel's memory is its own linear memory, so a bug in it
cannot reach the JVM's heap, which makes this the road for UNTRUSTED
plugins.

- **What the module is granted.** `WasmLib.load(bytes)` gives it a WASI
  that grants nothing: no files, no environment, no arguments. Its stderr
  goes to a buffer of its own. When a call TRAPS, the `Left` carries
  what the module wrote there (a Rust or Go panic's reason), not just
  "unreachable". A reactor module's `_initialize` is called once, at
  load.
- **Buffers.** A host cannot hand a module its own pointers, so the crate
  also exports `okay_alloc`/`okay_free`. Buffers live in the module's
  memory, and `withBuffers` frees every one after the call.
- **The handler.** `PasswordHash.wasm(lib)` is the effect's third handler:

```scala
  private def wasm: Handler[PasswordHash] = PasswordHash.wasm(lib)
```

- **The law holds here too.** Under Chicory the kernel gives
  BouncyCastle's bytes over the same 48 cases. They take about 4.5 s,
  because Chicory interprets, against about 1.3 s native. Refused
  parameters give the same `Left`, and a mutant that reads the output one
  byte off fails the law.

Go reaches the same road with `GOOS=wasip1 GOARCH=wasm`
([okay with Go](go.md)).

## The same kernel on Scala Native

okay-rust is a cross project, and on Scala Native the crate's
`staticlib` is linked into the binary. The call is an ordinary C call,
with no runtime between:

```scala
@extern object Argon2Kernel:
  def okay_argon2id(password: Ptr[Byte], passwordLen: CSize, salt: Ptr[Byte], saltLen: CSize,
                    memoryKib: CUnsignedInt, iterations: CUnsignedInt, parallelism: CUnsignedInt,
                    out: Ptr[Byte], outLen: CSize): CInt = extern
```

`PasswordHash.native` is that platform's handler. Every buffer is `malloc`'d
before the call and freed after it. The effect and the programs written
against it are shared code, and only the handlers are the platform's:
`PasswordHash.rust` and `PasswordHash.wasm` on the JVM, `PasswordHash.native` on Native.

BouncyCastle does not run on Native, so the law crosses platforms through
PINNED vectors:
- the JVM suite checks them against BouncyCastle, in the default gate;
- the same suite on Native holds the linked kernel to those bytes.

`scripts/rust-native-check.sh` does both steps: it builds the
`staticlib` offline and links it by its full path (on macOS `-l` would
pick the `.dylib` beside it), then runs the suite. A mutant that adds an
iteration on Native is caught.

## Go

Go is possible, and of the three the least worth doing IN-PROCESS.
`go build -buildmode=c-shared` exports a C ABI, but it loads a second
runtime into the JVM, with its own garbage collector, its own scheduler
and its own signal handlers. Go installs `SA_ONSTACK` handlers that the
JVM also wants, and a cgo call from a foreign thread costs a thread
switch. Most Go code is SERVICES, and a service's seam is the network,
which okay-http already speaks. So okay's roads to Go are:

1. a worker process on okay's wire, the way the TypeScript and Haskell
   workers run. This one is built: [okay with Go](go.md);
2. a Go plugin compiled to WebAssembly (`GOOS=wasip1`, no TinyGo needed),
   on the Chicory road above.

In-process `c-shared` is refused for the reasons above.

## Literature

- [JEP 454: Foreign Function & Memory API](https://openjdk.org/jeps/454), final in JDK 22.
- A. Biryukov, D. Dinu, D. Khovratovich, S. Josefsson. [RFC 9106: Argon2 Memory-Hard Function for Password Hashing and Proof-of-Work Applications](https://doi.org/10.17487/RFC9106), 2021.
- The [RustCrypto `argon2` crate](https://docs.rs/argon2/0.5.3), the kernel's implementation.
- Gordon Plotkin, Matija Pretnar. [Handling algebraic effects](https://doi.org/10.2168/LMCS-9(4:23)2013), LMCS 2013: why a kernel is an operation and its implementation a handler.

The design and its results: [specs/polyglot-rust.md](../specs/polyglot-rust.md).
