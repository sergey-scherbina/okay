# okay-scala2-prelude

okay's TOP-LEVEL names for a **Scala 2.13** program: a package object
for `okay.scala2`, compiled by scalac 2.13 itself. The facade
[`okay-scala2`](okay-scala2.md) is written in Scala 3, and scalac 2.13's
TASTy reader does not see a Scala 3 top-level definition, so these are
the names only a Scala 2 compiler can provide. With this module on the
classpath, one `import okay.scala2._` brings them together with the
rest of the facade — as `import okay2._` does on
[okay2](../okay2.md), and as the Scala 3 core spells them.

| | |
|---|---|
| `type +[R, S] = R with S` | a row of several capabilities: `State[Int] + Writer[String]` |
| `type ![A, R] = Eff[R, A]` | a program: `Int ! (State[Int] + Writer[String])` |
| `type %[F[_], A] = F[A]` | `State % Int` is `State[Int]` |
| `type Pure = Any` | the row that needs nothing |
| `pure(a)` | a value as a program |
| `choose(as: _*)`, `runChoice(p)` | okay's top-level nondeterminism |
| `!.run(p)` | run a program with nothing left to perform |
| `p.runWith` | run an `Async` program |

The build line is in [okay from Scala 2.13, section 1](../scala2.md#1-setting-up-the-build);
it is a `%%` dependency, because this artifact is Scala 2.13, and it
brings `okay-scala2` along. The same classpath arrangement applies (the
Scala 3 standard library behind 2.13's). The design is
specs/scala2-facade.md, stage 21.
