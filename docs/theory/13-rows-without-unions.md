# 13. Rows without unions — the same row, seen from the handler

## The row okay writes, and the language it needs

Chapter 5 set up rows as unions. A program's type is `A ! R`, and a row
is a union of signatures:

```scala
infix type +[F[+_], G[+_]] = [A] =>> F[A] | G[A]     // Effects.scala:69
```

A program in `State % Int + Writer % String` may perform an operation
of either signature. Membership is subtyping (`F[A] <: (F + G)[A]`),
and a handler splits the union by a runtime test, `TypeableK.test`
(Handler.scala:41), inside `split` (Handler.scala:283). Leijen's row
types \[[2014](#ref-leijen-2014)\] and Kiselyov and Ishii's open unions
\[[2015](#ref-kiselyov-2015)\] are the two ancestors. Scala 3 lets the
union BE the row, with no encoding in between.

Scala 2 has no union types. This chapter is about what a Scala 2 program
can use in their place, and why the answer `okay-scala2` gives is the
same row rather than an approximation of it.

## Three ways to write a row without a union

**A coproduct.** Swierstra's *data types à la carte*
\[[2008](#ref-swierstra-2008)\] writes the row as a nested sum,
`F :+: G :+: ...`, with an injection type class for membership. It
works in any language with type classes. What it costs is inference:
the order of the sum is visible in every type, and membership needs an
implicit search at each operation. It would also mean a second
representation of every program alongside the Scala 3 one.

**A type-level set of members.** The Haskell `extensible-effects`
family and its Scala ports keep the row abstract and pass `Member`
evidence around. This is the coproduct with the order hidden. The
evidence still has to be searched for, and Scala 2 cannot be taught to
search for membership of a Scala 3 union.

**An intersection of capabilities.** State the row from the other
side: not "the operations this program may perform" but "the handlers
this program requires". A program that may perform `F` or `G` requires
a handler for `F` AND a handler for `G`. So the union of operations is
an INTERSECTION of requirements. That is the familiar law for function
types, `(F ∨ G) → X ≅ (F → X) ∧ (G → X)`, which is sound for
intersection and union types in subtyping systems
\[[Dunfield & Pfenning 2003](#ref-dunfield-2003)\]. Scala 2 has
intersections (`with`), and a one-line alias in the user's own code,
`type +[R, S] = R with S`, lets the row be WRITTEN as chapter 5 writes
it, `State[Int] + Writer[String]` — the `+` of Scala 3 joins two
operation languages, this one two requirements, and the rest of this
chapter is why they are the same row. Brachthäuser, Schuster and Ostermann build a
whole effect language on this reading, *effects as capabilities*
\[[2020](#ref-brachthauser-2020)\], and ZIO 1's environment `R` is the
same shape in Scala 2 practice.

`okay-scala2` takes the third road, and it gives up nothing to the
first two, because it is the SAME row.

## Contravariance does the membership proof

```scala
final class Eff[-R, A] private (private val body: EffBody[A])      // okay-scala2 Eff.scala:26
```

`R` is a requirement, so `Eff` is CONTRAVARIANT in it. Requiring less
is a subtype of requiring more:
`A ! State[Int] <: A ! (State[Int] + Writer[String])`. So a
single-effect operation fits into any wider program without an
injection, a widening call, or a search. That is exactly what
`Row.In` gives the Scala 3 API, and here subtyping gives it for
free. `flatMap[R1 <: R, B]` (Eff.scala:32) lets scalac find the smallest
row that satisfies both sides of a bind.

A handler takes one capability off:

```scala
def run[S, R, A](s: S)(e: Eff[State[S] & R, A]): Eff[R, (S, A)]   // Eff.scala:88
```

and running requires that nothing is left:

```scala
def run[A](e: Eff[Any, A]): A                                        // Eff.scala:68
```

`Any` is the empty intersection. A program with a capability still in
its row is not an `A ! Any`, by contravariance, so forgetting a
handler is a type error. That is the discharge discipline of chapter
5's handlers (Plotkin & Pretnar \[[2009](#ref-plotkin-2009)\]), stated
with nothing but variance.

## The phantom, and the one cast

The capability types are phantoms. `State[S]`, `Writer[W]` and
`Effect[F]` have no values, and `R` never meets a Scala 3 type.
Kiselyov and Shan call such a type a *lightweight static capability*
\[[2007](#ref-kiselyov-shan-2007)\]: a type that proves something
statically and is gone at run time.

Underneath, every program is stored at ONE row, `Top[+X] = Any`
(Eff.scala), and each handler re-types it at the concrete union it
handles. That re-typing is a cast, `Rows.coerce` (Eff.scala:56), and it
is the only way to connect the two sides. `Free` is invariant in its
row, and `R` has no Scala 3 counterpart to carry. The cast is sound for
two reasons, each checked in its own place:

- **At run time a row is only a class test.** A handler finds its
  operations with `TypeableK.test` (Handler.scala:41), which looks at
  the operation's class and never at the row type. So storing the
  program at `Top` changes nothing the handlers look at.
- **Statically, `R` lists every capability.** `Eff.run` accepts only
  `A ! Any`, so by the time the tree is walked, every operation in
  it has had a handler applied.

A Scala 2 user's own effect follows the same pattern one level down.
`Effect[F]` builds `TypeableK.test` from a `ClassTag`
(Effect.scala:38), so `derives Effect`, a Scala 3 derivation, is not
needed. After the test accepts an operation, one more cast, `narrow`
(Effect.scala:44), types it as `F`. It sits next to the test that
justifies it.

## Handlers are still handlers

A Scala 2 `Handler[F, R, B]` receives an operation and its continuation
`k`, as in chapter 5:

```scala
def apply[X](op: F[X], k: X => Eff[R, B]): Eff[R, B]
```

Underneath, it is a `shift` into the continuation of chapter 2
(Effect.scala:50). So the Scala 2 handler has the full power of a deep
handler: resume once, not at all (abort), or several times
(nondeterminism). The probe exercises all three from Scala 2.13
(`TestOwnEffectFromScala2`). Kammar, Lindley and Oury
\[[2013](#ref-kammar-2013)\] catalogue what that power buys.
Continuations themselves are exposed as `Cont[A, S, R]`, chapter 3's
parameterised monad, answer-type modification included.

## What the Scala 2 reader forced, and what it did not

The design above is the theory. A handful of facts about scalac 2.13
and its TASTy reader decided the rest. Each one was measured, and each
is recorded with its error message in specs/scala2-facade.md. The
first three shaped the core facade:

- **It refuses `inline`.** "Unsupported Scala 3 inline method flatMap;
  found in class okay.Free". So okay's own combinators cannot be
  called from Scala 2 at all. This is why the facade is a set of
  classes, not a set of type aliases.
- **It reads constructor parameter types eagerly.** A union there
  refuses the whole class, and the error points at the user's
  `package` line. This is why the program lives in a value class
  (`ProgBody`, Prog.scala:69) behind each facade class. Methods are read
  only when called, so they may mention the union freely.
- **It infers `R = Any` at the last handler of a curried `handle`.**
  `-Xlint` reports that as an error-worthy inference. So a user
  effect's last handler is `run` (Effect.scala:62), which has no `R`.

The rest turned up while wrapping the libraries above the core (codecs,
HTTP, SQL, agents, UI, WebSockets). They are the same kind of fact, one
level up:

- **Some TASTy it cannot read at all.** `okay.codec.Json`'s crashes it
  ("class file ... is broken (class scala.MatchError/49)"). The damage
  is to every SIGNATURE that names `Json`, not only to `Json` itself,
  so the facade speaks JSON as text, and holds a `Json` it must keep
  in a value class (`FormValue`) for the constructor reason.
- **Scala 3 generic tuples are refused.** okay-http's `Route` builds its
  parameters as `*:` tuples ("Unsupported Scala 3 generic tuple type
  scala.Tuple"). Scala 2 routing is pattern matching instead, over
  okay-http's own decoding (`okay.http.Urls`).
- **Top-level definitions are invisible.** A Scala 3 `type` alias at
  package level cannot be seen from 2.13 at all ("type Chunk is not a
  member of package okay"). What the alias NAMES can be seen. `Chunk`
  is `ArraySeq`, and a Scala 2 caller passes an `ArraySeq` where a
  `Chunk` is asked for. So `Schema` keeps its okay-codec name.
- **Names in the facade's package are read while resolving names.** An
  internal class named `Body` made scalac read it, and refuse it, while
  resolving `okay.http.Body` in a file that imported both packages. So
  internal classes do not take common names (`ProgBody`), and the loop
  object is `UiApp`, because `App` would capture `object Main extends
  App`.
- **An enum case is typed as the case.** Scala 3 widens
  `Event.Pressed("inc")` to `Event`, and Scala 2 does not. So an
  invariant container of events needs its type argument written out.
- **Implicit search DOES find Scala 3 givens.** `Schema[Int]`, and even
  `Schema[Option[Vector[Long]]]` through by-name `using` clauses, resolve
  from Scala 2. That is why most of okay-codec needed no facade.

The general lesson is the one the facade was built on. A wrapper is
written only for what the reader cannot use, because each area was
probed before it was wrapped, and most of each library's DATA turned
out to be usable as it is.

## What this does not claim

- **Not that intersections are a better row.** In Scala 3 the union
  is the direct statement, and the Scala 3 API keeps it. The
  intersection is the same row seen from the handler's side, and it is
  used because it is what Scala 2 can write.
- **Not that the facade is as fast.** A Scala 2 caller cannot reach
  okay's `inline` fast paths, and that cost has not been measured.

## References

- <a id="ref-leijen-2014"></a>Daan Leijen. *[Koka: Programming with row polymorphic effect types.](https://doi.org/10.4204/EPTCS.153.8)* MSFP 2014.
- <a id="ref-kiselyov-2015"></a>Oleg Kiselyov, Hiromi Ishii. *[Freer monads, more extensible effects.](https://doi.org/10.1145/2804302.2804319)* Haskell Symposium 2015.
- <a id="ref-swierstra-2008"></a>Wouter Swierstra. *[Data types à la carte.](https://doi.org/10.1017/S0956796808006758)* Journal of Functional Programming 18(4):423–436, 2008.
- <a id="ref-dunfield-2003"></a>Jana Dunfield, Frank Pfenning. *[Type assignment for intersections and unions in call-by-value languages.](https://doi.org/10.1007/3-540-36576-1_16)* FoSSaCS 2003, LNCS 2620.
- <a id="ref-brachthauser-2020"></a>Jonathan Immanuel Brachthäuser, Philipp Schuster, Klaus Ostermann. *[Effects as capabilities: effect handlers and lightweight effect polymorphism.](https://doi.org/10.1145/3428194)* OOPSLA 2020.
- <a id="ref-kiselyov-shan-2007"></a>Oleg Kiselyov, Chung-chieh Shan. *[Lightweight static capabilities.](https://doi.org/10.1016/j.entcs.2006.10.039)* Electronic Notes in Theoretical Computer Science 174(7):79–104, 2007.
- <a id="ref-plotkin-2009"></a>Gordon Plotkin, Matija Pretnar. *[Handlers of algebraic effects.](https://doi.org/10.1007/978-3-642-00590-9_7)* ESOP 2009.
- <a id="ref-kammar-2013"></a>Ohad Kammar, Sam Lindley, Nicolas Oury. *[Handlers in action.](https://doi.org/10.1145/2500365.2500590)* ICFP 2013.

---

← [10 · Optics on profunctors](10-optics.md) · [Contents](index.md)
