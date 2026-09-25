# 16 · Continuations and monads

> **Part IV is about building.** Compiled in
> `src/test/scala/TestBookMonads.scala`. This chapter is about a
> result from 1994 that most working programmers have never heard of,
> and that every one of them has used. It is the reason `!` exists.

---

## The claim

Andrzej Filinski, *Representing Monads*, POPL 1994:

> Given delimited control, any monad can be embedded in direct style.

Read that as a programmer rather than as a reader of proceedings. It
says: **you never have to write `flatMap`.** Not "there is a macro for
your favourite three monads" — *any* monad, including one you invent
this afternoon, including one whose author never heard of this
library. Direct style is not a privilege that a library grants to the
effects it happens to know about. It is a consequence of having
delimited control, and it is available to everything.

Every `direct` block in this book from chapter 5 onward works for this
reason. That is the whole chapter. The rest of it is evidence, because
a claim this large should not be taken on the strength of a citation.

## Evidence from the top: a monad the library never heard of

`Option` is not one of this library's effects. It has no row, no
handler, no entry in any table. All it has is a `Monad` instance,
which the *user* writes:

```scala
given Monad[Option] with
  override def pure[A](a: A): Option[A] = Some(a)
  extension [A](m: Option[A])
    override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)
```

That is the entire admission fee. Here is `Option` in direct style:

```scala
def add(mx: Option[Int], my: Option[Int]): Option[Int] =
  direct[Option] {
    val x: Int = mx
    val y: Int = my
    x + y
  }
```

`add(Some(2), Some(3))` is `Some(5)`. `add(Some(2), None)` is `None` —
and so is `add(None, Some(3))`, which is the interesting one: the
second operand is *not* evaluated after the first said `None`. Nothing
in that function body says so. The short-circuit is the monad's, and
it survives the translation intact.

To make "any monad" more than a figure of speech, the test file
invents one on the spot — a monad that counts its own binds:

```scala
final case class Counted[A](value: A, steps: Int)

given Monad[Counted] with
  override def pure[A](a: A): Counted[A] = Counted(a, 0)
  extension [A](m: Counted[A])
    override def flatMap[B](f: A => Counted[B]): Counted[B] =
      val next = f(m.value)
      Counted(next.value, m.steps + next.steps + 1)
```

and runs it in direct style:

```scala
val inDirect = direct[Counted] {
  val a: Int = step(2)
  val b: Int = step(3)
  a * b
}
val byHand = step(2).flatMap(a => step(3).map(b => a * b))
```

The test asserts `inDirect == byHand` — not just the value but the
step count. The direct block is the `flatMap` chain, with the same
bookkeeping, not an approximation of it.

This is the half of the theorem that a reader of this library actually
consumes. Anything with a `Monad` instance can be written straight
down.

## Evidence from the bottom: a monad built out of `shift`

The other direction is more surprising and more convincing. If
delimited control can *represent* any monad, then it should be
possible to take a monad everybody knows, throw away its
implementation, and rebuild it out of nothing but `shift`.

The state monad, in full:

```scala
def sGet[S, R]: Cont[S, S => R, S => R]           = shift(k => s => k(s)(s))
def sSet[S, R](s2: S): Cont[Unit, S => R, S => R] = shift(k => _ => k(())(s2))
def sRun[S, A](s: S)(m: Cont[A, S => (S, A), S => (S, A)]): (S, A) =
  (m / (a => (fin: S) => (fin, a)))(s)
```

Two operations and a runner. There is **no store**. No cell, no
`var`, no threading parameter written by hand, no `S => (S, A)` data
type declared anywhere. Look at what `sGet` does: it captures the rest
of the program as `k`, and returns a *function waiting for a state*.
When that function finally receives `s`, it calls the rest of the
program with `s` as the answer to the `get` — and hands it `s` again
as the store to continue with. `sSet` is the same shape with one
difference: it passes `s2` onward instead of `s`. The state is the
continuation's argument. That is the whole trick.

It works:

```scala
val r = sRun(1) {
  for
    a <- sGet[Int, (Int, Int)]
    _ <- sSet[Int, (Int, Int)](a + 10)
    b <- sGet[Int, (Int, Int)]
  yield a + b
}
// r == (11, 12)
```

And — this is the part worth pausing on — it obeys the state laws,
which nobody told it about:

```scala
// get >>= set  ==  pure(())
sRun(7) { sGet.flatMap(s => sSet(s)) }              // (7, ())
// set x >> get  ==  set x >> pure(x)
sRun(7) { sSet(9).flatMap(_ => sGet) }              // (9, 9)
```

Those laws are not asserted by the implementation. They fall out of
the way `shift` passes an argument along. The state monad was not
*modelled* here; it was *derived*, and it came out correct because the
continuation already had the right shape.

Do this with `Reader` (pass a fixed environment instead of a changing
one), with `Writer` (accumulate on the way back — chapter 8), with
`List` (call `k` more than once — chapter 13), with exceptions (do not
call `k` at all — chapter 5). Each is a few lines and each is the same
move. That is Filinski's theorem, from the bottom.

## Where the library's own road meets it

The library ships a `State` *effect*, with a row and a tail-recursive
handler. Same program:

```scala
val p: Int ! State % Int = for
  a <- State.get[Int]
  _ <- State.set(a + 10)
  b <- State.get[Int]
yield a + b

State.run[Int, Int](1)(p)   // (11, 12) — the same answer
```

So why does the effect exist, if four lines of `shift` do the job? For
the ordinary engineering reasons, and it is worth being blunt about
them, because "it's all continuations underneath" is true and is not
an argument for writing it that way:

- The handler loop is **tail-recursive**. The derived version costs a
  stack frame per operation, and it is slower: **1.29x** on the same
  workload (`HandlerBenchmark`, 1000 get/set pairs — 21.23 µs/op for
  the effect against 27.42 µs/op for the derived monad, 3 forks,
  measured 2026-09-17). Worth knowing before you reach for the
  four-line version in a hot loop; not worth much anywhere else.
- The **type** is `Int ! State % Int`. It names one effect. The
  derived version's type is `Cont[Int, Int => (Int, Int), Int => (Int, Int)]`,
  which says "a continuation whose answer type is a function" — true,
  and not what the programmer wants to read in an error message.
- Effects **compose by row**. Two `Cont` monads with different answer
  types compose by hand, if at all.

This is chapter 14's rule seen from the other side. Chapter 14 said:
*if it can be an effect, it should be.* Here is the reason in one
sentence — **the capture is how you build it, the effect is what you
ship.** Filinski's theorem tells you the road is always open; it does
not tell you to camp on it.

## The generalisation you get for free

Because the store is just an argument, nothing forces it to keep its
type. The library's `PState` is the same two definitions with the
types loosened, and it gives you state whose *type changes* mid-program:

```scala
val r = PState.run(41):
  for
    n <- PState.get                    // n: Int
    _ <- PState.set((n + 1).toString)  // the state is a String now
    s <- PState.get                    // s: String
  yield s + "!"
// r == ("42", "42!")
```

That is Atkey's parameterised state, and it is a typestate: the
compiler enforces the protocol order, because reading the state after
the write *cannot* give you an `Int` — the type has moved on. Try to
build that on a mutable cell and you will find the cell has one type
forever.

This is worth noticing as a pattern, not just a feature. The derived
version was more general than the primitive one, and not because
anybody worked at it. Deriving a thing from continuations tends to
hand you a knob the hand-written version did not have, because the
continuation's argument was always free to vary and only the
implementation was holding it still.

## What this does and does not buy you

It does not make the monad disappear. `direct[Counted]` still produces
a `Counted`; the type is in the signature; short-circuiting still
short-circuits. Direct style changes **how you write the composition**,
not what the composition means. If the monad's `flatMap` is wrong, the
direct block is wrong in exactly the same way — and the chapter's test
asserts the agreement rather than assuming it.

It also does not make everything a monad. Chapter 11's four captures
include `control`/`control0`, which are not monadic operators at all,
and chapter 13's multi-shot answers a question monads answer badly.
Filinski's result is a lower bound on what delimited control can do,
not a ceiling.

What it does buy is the thing the first four chapters were arguing
for. Every argument in this book about writing the straight-line
version of a program rests on being *allowed* to — and the permission
is not a language feature, a compiler special case, or a list of
blessed types. It is a theorem, it is thirty years old, and the four
lines above are its proof by construction.

---

← [15 · Resumable exceptions](15-resumable-exceptions.md) ·
[Contents](index.md) ·
[16b · Two monads at once →](16b-two-monads-at-once.md)
