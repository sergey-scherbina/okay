# okay-kernel

A microkernel (specs/kernel.md): the half of composition the compiler
cannot do — a set of parts known only when the program starts, each
built separately, possibly against another version of the contract it
implements. For parts known at compile time, keep `Module` (the core's
Provide.scala), which proves more.

**Depends on:** the core only.

| | |
|---|---|
| `Version`, `Range` | SemVer and what a requirer accepts: `^1.2`, `=1.2.3`, `>=1.2 <2`, `*` |
| `Port[A]` | a contract: a name, its version, `One` or `Many`, and laws every implementation must pass |
| `Plugin` | an id, a version, the kernel range it was built for, `needs` and `provides` |
| `Provision[A]` | an implementation of a port, the contract version it was BUILT against (a literal), made as a `Resource` |
| `Kernel.plan` | every problem at once — missing, incompatible, unserved, ambiguous, cycle, duplicate, kernel mismatch — or a start order |
| `Kernel.start` / `assemble` | make every provision in order, laws checked, released in reverse |
| `Discover.services` / `jars` | JVM: `META-INF/services/okay.kernel.Plugin`, and a plugins directory; a broken provider is a `LoadFailed`, not a crash |
| `OkayModules` (okay-deploy's sbt plugin) | forbidden module edges, refused when the build loads |

## A port, two plugins, a start

```scala
import okay.kernel.*

trait Clock { def now: Long }
val clock: Port[Clock] = Port.one("clock", Version("1.2"))

class SystemClock extends Plugin:
  def id = "system-clock"
  def version = Version("1.0")
  def needs = Vector.empty
  def provides = Vector(Provision.value(clock, "1.2")(_ => new Clock { def now = System.currentTimeMillis }))

class Stamper extends Plugin:
  def id = "stamper"
  def version = Version("1.0")
  def needs = Vector(Need.of(clock, "^1.2"))
  def provides = Vector.empty
```

`Kernel.plan(Seq(SystemClock(), Stamper()))` answers the order
(`system-clock`, then `stamper`) or every problem; `Kernel.assemble(...)`
under `Resource.open` starts them and hands back a `Running` that reads
any port.

## The version contract

A provision says the contract version it was compiled against. On a
host whose port is `h`: another major, or newer than `h`, is
`Incompatible`; a need's range must accept what was built. A plugin
built for another kernel API is `KernelMismatch`.
