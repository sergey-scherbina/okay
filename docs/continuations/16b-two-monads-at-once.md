# 16b · Two monads at once

> **Part IV is about building.** Compiled in
> `okay-direct/src/test/scala/TestBookTwoMonads.scala` and
> `okay-cats/src/test/scala/bookcats/TestBookTwoMonadsCats.scala`. Chapter 16
> showed that any ONE monad can be written in direct style. This
> chapter is about the question every real program asks next: what
> about two? It shows the problem, then ONE real program written three
> ways — cats' monad transformers, layered monadic reflection
> (Filinski, and Biernacki's `$` and `shift0`), and algebraic effects —
> and what each one means.

---

## The problem, in one example

Two ordinary effects. A lookup that can fail:

```scala
def lookup(x: Int): Option[Int] = if x == 2 then None else Some(x * 10)
```

and a computation that has several answers, a `List`. Each on its own
is easy, because each is a monad and a for-comprehension handles it:

```scala
val opt = for { a <- lookup(1); b <- lookup(3) } yield a + b
val lst = for { x <- List(1, 2); y <- List(10, 20) } yield x + y
```

`opt` is `Some(40)`, `lst` is `List(11, 21, 12, 22)`. Now the program
we actually want: for every `x` in a list, look it up, and add. The
answer is a `List[Option[Int]]` — one result per element, some of them
missing. And the for-comprehension no longer helps, because its
`flatMap` belongs to ONE monad. So the plumbing is written by hand:

```scala
val byHand: List[Option[Int]] =
  List(1, 2, 3).map { x =>
    lookup(x) match
      case Some(y) => Some(x + y)
      case None    => None
  }
```

It gives `List(Some(11), None, Some(33))`, and it is fine at this size.
It stops being fine the moment there are three lookups, a second list,
or an `Either` for errors: every step re-states the `match`, and the
`match` is exactly the part people get wrong (chapter 2's "the layer
that forgets to propagate").

## Why monads do not compose by themselves

A monad is a type constructor with `pure` and `flatMap`. Two of them,
`List` and `Option`, give a new type constructor `List[Option[_]]` for
free. The question is whether that is a monad again — whether its
`flatMap` can be written once, for ANY two monads, without knowing
which. Try it:

```text
flatMap(m: M[N[A]])(f: A => M[N[B]]): M[N[B]]
  m.flatMap { na =>        // na: N[A]
    na.map(f)              // N[M[N[B]]]  ... but M's flatMap needs M[N[B]] back
  }
```

To finish, you need to turn an `N[M[X]]` inside out into an
`M[N[X]]` — to "swap" the two layers. Nothing in the definition of a
monad gives you that swap. Some pairs have one (Jones and Duponcheel
called it a *distributive law*, 1993); many do not; and none of the
generic code can find it. That is the whole of the famous sentence
"monads do not compose": functors compose, applicatives compose, and
monads need one extra piece of glue per pair.

## The standard answer: monad transformers

Instead of looking for the swap, write it once per monad, on the
INSIDE, and parameterise over the outside. A transformer is "monad
`N`, living inside any monad `F`". The one everybody writes first is
Option inside something else:

```scala
final case class OptionT[F[_], A](run: F[Option[A]]):
  def flatMap[B](f: A => OptionT[F, B])(using F: Monad[F]): OptionT[F, B] =
    OptionT(run.flatMap {
      case Some(a) => f(a).run
      case None    => F.pure(None)
    })
```

Read `flatMap` carefully, because it is the whole trick: it runs the
OUTER monad's `flatMap` (here, `List`'s), and for every element it
either continues with `f` (`Some`) or stops that branch with a `None`.
That `match` is the swap from the previous section, written by hand for
`Option` and nothing else. The outside can be any monad, which is what
makes it reusable. Values of the outer monad get into the stack with a
`lift`:

```scala
def lift[F[_], A](fa: F[A])(using F: Monad[F]): OptionT[F, A] =
  OptionT(fa.flatMap(a => F.pure(Some(a))))
```

And the program is one for-comprehension again:

```scala
val viaT: OptionT[List, Int] =
  for
    x <- OptionT.lift(List(1, 2, 3))
    y <- OptionT(List(lookup(x)))
  yield x + y
```

`viaT.run` is `List(Some(11), None, Some(33))`: the same answer as the
hand-written version, with the plumbing moved into the transformer.

**The order is a different transformer.** "A list of optional answers"
and "an optional list of answers" are different programs. The second
one — one `None` anywhere empties the whole result — is `List` living
inside `Option`, and that is a second class with its own `flatMap`:

```scala
final case class ListT[F[_], A](run: F[List[A]]):
  def flatMap[B](f: A => ListT[F, B])(using F: Monad[F]): ListT[F, B] =
    ListT(run.flatMap { as =>
      as.foldRight(F.pure(List.empty[B])) { (a, rest) =>
        f(a).run.flatMap(bs => rest.flatMap(cs => F.pure(bs ++ cs)))
      }
    })
```

```scala
val viaT: ListT[Option, Int] =
  for
    x <- ListT(Option(List(1, 2, 3)))
    y <- ListT.lift(lookup(x))
  yield x + y
```

This `viaT.run` is `None`. Without the failing element (`List(1, 3)`)
it is `Some(List(11, 33))`.

**What transformers cost.** They work, and whole ecosystems are built
on them. The price is the reason the rest of this chapter exists:

- **a class per monad, and per ORDER.** `OptionT` and `ListT` are
  different code; so are `EitherT`, `StateT`, `WriterT`. A program's
  effect is the stack of them, spelled in its type
  (`EitherT[StateT[OptionT[IO, *], S, *], E, A]` is not a made-up
  example).
- **`lift` at every layer boundary.** A value from two layers down is
  `lift(lift(x))`. Libraries add type classes (`MonadState`,
  `MonadError`, "mtl style") to hide it, and pay with an instance for
  every transformer times every class.
- **subtle laws.** The `ListT` above is the famous "ListT done wrong":
  it is only a lawful monad when the inside is commutative. It is fine
  over `Option`; over `State` it is not.
- **a wrapper and an extra `flatMap` per bind**, at run time.

## One real program, three ways

The two classes above are toys. From here on the chapter follows ONE
program with three effects and writes it three times: with cats' monad
transformers, with layered monadic reflection, and with algebraic
effects. All three live in
`okay-cats/src/test/scala/bookcats/TestBookTwoMonadsCats.scala`, and a
test asserts that they give the same answer.

The program buys a basket. It may buy in either of two shops and wants
to see BOTH outcomes (a `List` of choices); a shop may not stock an item
(an error, `Either`); and every step goes to a log (`Writer`). The price
list:

```scala
val prices: Map[String, Map[String, Int]] = Map(
  "north" -> Map("tea" -> 300, "cake" -> 500),
  "south" -> Map("tea" -> 250))
```

For `List("tea", "cake")` the answer we want is one entry per shop,
each with its own log and its own outcome:

```scala
val expected = List(
  (Vector("shop north", "total 800"), Right(800)),
  (Vector("shop south"), Left("south has no cake")))
```

### 1. Monad transformers (cats)

First the stack is named, one layer per type, from the inside out:

```scala
type Branches[A] = WriterT[List, Vector[String], A]
type App[A]      = EitherT[Branches, String, A]
```

Then every operation is written for its POSITION in that stack. A
choice lives two layers down, so it is lifted twice; the log one layer
down, so once:

```scala
def choose[A](xs: List[A]): App[A] =
  EitherT.liftF(WriterT.liftF(xs))

def log(msg: String): App[Unit] =
  EitherT.liftF(WriterT.tell[List, Vector[String]](Vector(msg)))

def price(shop: String, item: String): App[Int] =
  EitherT.fromOption[Branches](prices(shop).get(item), s"$shop has no $item")
```

The program reads well — that is what the stack buys:

```scala
def basket(items: List[String]): App[Int] =
  for
    shop  <- choose(List("north", "south"))
    _     <- log(s"shop $shop")
    ps    <- items.traverse(item => price(shop, item))
    total  = ps.sum
    _     <- log(s"total $total")
  yield total
```

and it is run by peeling the layers off, outermost first:

```scala
basket(items).value.run
```

What it cost:

- **Every helper knows the whole stack.** `choose` says
  `EitherT.liftF(WriterT.liftF(…))` because a list is exactly two layers
  down. Add a layer and every lift in every helper changes.
- **The order is the meaning, and it is frozen into the type.** Here
  the error is INSIDE the log and the log inside the choice, so the
  south branch keeps its log and fails alone. "One missing price fails
  the whole basket" is a different program: a different `App`, and
  every function written against `App` rewritten.
- **Three type aliases** and a run expression that is the stack spelled
  backwards, before a line of business logic.

### Stacks do not compose with each other

That was ONE stack. The sharper problem shows up the day two pieces of
code written against DIFFERENT stacks meet — and the difference that
hurts most is not the order of the layers but what the stacks are MADE
of. Two teams, one shared effect (errors), and one monad each that the
other does not have:

- **team A** knows the catalog: a shop sells several varieties of an
  item, so a price comes with the variety it is for — a list, and an
  error for an item the shop does not sell:

```scala
type Choices[A] = EitherT[List, String, A]

def priceOf(shop: String, item: String): Choices[(String, Int)] =
  EitherT(catalog(shop).get(item) match
    case Some(varieties) => varieties.map(Right(_))
    case None            => List(Left(s"$shop has no $item")))
```

- **team B** knows delivery, which a shop may simply not offer — an
  `Option` (absent is not an error) and an error (an unknown shop is):

```scala
type Checked[A]   = Either[String, A]
type Delivered[A] = OptionT[Checked, A]

def deliveryFee(shop: String): Delivered[Int] =
  OptionT(fees.get(shop).toRight(s"unknown shop $shop"))
```

There is no ordering to agree on. Call both in one expression:

```text
for
  fee          <- CatsDelivery.deliveryFee("north")
  (tea, price) <- CatsDelivery.priceOf("north", "tea")
yield (tea, price + fee)
```

and it does not compile:

```text
Found:    cats.data.EitherT[List, String, (String, Int)]
Required: cats.data.OptionT[bookcats.CatsDelivery.Checked, B]
```

The order needs all three monads, so it needs a THIRD stack, the union,
which neither team wrote, and each team's helpers get into it through a
conversion written by hand, one per team — per pair of stacks, in
general:

```scala
type Order[A] = OptionT[Choices, A]

def fromA[A](fa: Choices[A]): Order[A]   = OptionT.liftF(fa)
def fromB[A](fb: Delivered[A]): Order[A] = OptionT(EitherT(List(fb.value)))
```

and every call site says which team it came from:

```scala
fee          <- fromB(deliveryFee(shop))
(tea, price) <- fromA(priceOf(shop, item))
```

For `"tea"` over both shops the order is
`List(Right(Some(("green tea", 350))), Right(Some(("black tea", 330))),
Right(None))`: north delivers and has two teas, south does not deliver.
Now multiply: every new team with a slightly different set of monads is
one more stack, one more union for everybody who combines it, and one
more conversion into each union — each knowing the structure of both
stacks, and changing when either does.

The two roads below have none of this. With layered reflection each
team's helper returns a plain value of plain monads, and the order is
three `reify` blocks:

```scala
def priceOf(shop: String, item: String): Either[String, List[(String, Int)]] =
  catalog(shop).get(item).toRight(s"$shop has no $item")
```

```scala
reify[List, Out, Pure]:
  reify[Checked, Option[(String, Int)], Pure]:
    reify[Option, (String, Int), Pure]:
      for
        shop         <- List("north", "south").reflect[Out, Pure]
        fee          <- deliveryFee(shop).reflect[Option[(String, Int)], Pure]
        f            <- fee.reflect[(String, Int), Pure]
        varieties    <- priceOf(shop, item).reflect[Option[(String, Int)], Pure]
        (tea, price) <- varieties.reflect[Out, Pure]
      yield (tea, price + f)
```

With algebraic effects each helper declares only the effects it uses.
Team A's varieties are a `choose`; team B's "no delivery" is not an
effect at all — it is a plain `Option` value, and only the unknown shop
is an error:

```scala
def priceOf(shop: String, item: String): (String, Int) ! Choose + Throws % String =
  catalog(shop).get(item) match
    case Some(varieties) => choose(varieties*).at[Choose + Throws % String]
```

```scala
def deliveryFee(shop: String): Option[Int] ! Throws % String =
  fees.get(shop) match
    case Some(fee) => pure[Throws % String, Option[Int]](fee)
    case None      => raise[String, Option[Int]](s"unknown shop $shop")
```

The union is just the row of the program that uses both helpers, each
widened into it, and the program looks at the `Option` itself:

```scala
type Order = Choose + Throws % String
```

```scala
fee  <- deliveryFee(shop).at[Order]
out  <- fee match
  case None    => pure[Order, Option[(String, Int)]](None)
  case Some(f) => priceOf(shop, item).map((tea, price) => Some((tea, price + f)))
```

And the one expression cats refused is, with effects, just written
down — both helpers in one `for`:

```scala
val north: Option[(String, Int)] ! Order =
  for
    fee          <- deliveryFee("north").at[Order]
    (tea, price) <- priceOf("north", "tea").at[Order]
  yield fee.map(f => (tea, price + f))
```

All three give the same answer (`TestBookTwoMonadsCats`).

**The same effects in another order are no better.** Agreeing on the
set of monads is not enough; the order is part of the stack too.

Take team X with the basket's own stack and team Y with the same three
effects in the other order, the error OUTSIDE the log, plus team Z with
one more layer on top:

- **team X** wrote the basket above: `App` = error inside log inside
  choice;
- **team Y** wrote an audit helper with the same effects in the other
  order, the error OUTSIDE the log:

```scala
type Checked[A] = EitherT[List, String, A]
type Audited[A] = WriterT[Checked, Vector[String], A]

def audit(msg: String): Audited[Unit] =
  WriterT.tell[Checked, Vector[String]](Vector(msg))
```

- **team Z** needs a configuration too, so it put a `ReaderT` on top of
  team X's stack:

```scala
type Configured[A] = ReaderT[App, Config, A]
```

Team X tries to call team Y's `audit` inside its basket:

```text
for
  _ <- CatsStacks.audit("checked")
  t <- CatsBasket.price("north", "tea")
yield t
```

and the compiler refuses — two monads that mean "a list of logged,
possibly failed values" are two unrelated types:

```text
Found:    cats.data.WriterT[bookcats.CatsStacks.Checked, Vector[String], U]
Required: bookcats.CatsBasket.App[Int]
```

Team Z tries to call team X's `price` and is refused the same way,
because one more layer is one more type. The fix there is mechanical —
one more lift, in every helper it reuses:

```scala
def priced(shop: String, item: String): Configured[Int] =
  ReaderT.liftF(CatsBasket.price(shop, item))
```

Team X's fix for team Y's helper is not mechanical at all. Changing the
ORDER of two layers means writing the swap from the start of the
chapter by hand, for this one pair:

```scala
def reorder[A](fa: Audited[A]): App[A] =
  EitherT(WriterT(fa.run.value.map {
    case Right((log, a)) => (log, Right(a))
    case Left(e)         => (Vector.empty, Left(e))
  }))
```

And it cannot be written without losing something. In team Y's order an
error DISCARDS the log, so `audit("checked")` followed by a failure
runs to `List(Left("no stock"))` — the line is gone before `reorder`
ever sees it, and the converted value is `(Vector(), Left("no stock"))`
where team X's own order would have kept `Vector("checked")`. The two
stacks are not two spellings of one thing; they are different
programs, and there is no lossless function from one to the other.

So a transformer stack is not only frozen for one program. It is a
dialect: a helper speaks the stack it was written in, reuse across
stacks needs lifts (a different layer count) or a hand-written, possibly
lossy conversion (a different order), and a library of helpers has to
pick one stack for all its users. The two roads below have no such
thing to pick. A layered helper is a plain value — `Logged(Vector(msg),
())` works under any nesting of `reify` blocks. An effect helper is
written against the effects IT uses, and the union is nothing but the
row of the program that uses it — a price helper and a log helper widen
into the basket's row with `.at`, no conversion, no stack to agree on:

```scala
def priceOf(shop: String, item: String): Int ! Throws % String =
  prices(shop).get(item) match
    case Some(p) => pure[Throws % String, Int](p)
    case None    => raise[String, Int](s"$shop has no $item")

def note(msg: String): Unit ! Writer % String =
  Writer.tell(msg)
```

```scala
_     <- note(s"shop $shop").at[Basket]
ps    <- items.foldLeft(pure[Basket, List[Int]](Nil))((acc, item) => acc.flatMap(xs => priceOf(shop, item).at[Basket].map(xs :+ _)))
```

The same holds for a bigger row. A helper written against its one
effect:

```scala
def audit(msg: String): Unit ! Writer % String =
  Writer.tell(msg)
```

and is used, unchanged, in the basket's row, in a bigger row with a
configuration, and under either handler order:

```scala
type Taxed = okay.Reader % Int + Basket

def taxed(items: List[String]): Int ! Taxed =
  for
    vat   <- okay.Reader.ask[Int].at[Taxed]
    total <- basket(items).at[Taxed]
    _     <- audit(s"vat $vat%").at[Taxed]
  yield total * (100 + vat) / 100
```

## Filinski 1994: direct style, one monad per block

Chapter 16 showed Filinski's result: with `shift` and `reset`, any
monad runs in direct style — `reflect` takes the rest of the block as a
function `k` and hands it to the monad's `flatMap`, and `reify` is the
delimiter that collects the answer. Applied to our problem:

```scala
val viaFilinski: OptionT[List, Int] =
  reify:
    for
      x <- OptionT.lift(List(1, 2, 3)).reflect
      y <- OptionT(List(lookup(x))).reflect
    yield x + y
```

It works, and it gives the same `List(Some(11), None, Some(33))`. But
look at what is still there: `OptionT`, and `lift`. Direct style did not
remove the transformer; it only removed the `flatMap` calls. The reason
is structural. `Cont` has ONE prompt, so a `reify` block has ONE answer
type, so it can reflect ONE monad — and to put two into one block, that
one monad has to already be the stack.

## Filinski 1999: layers, and the ladder

Filinski saw this, and in *Representing Layered Monads* (POPL 1999) he
gave each layer its own `reflect` and `reify`. With plain `shift` and
`reset` that needs one delimiter per layer, and one delimiter is all
`reset` offers. The classic way out is the CPS hierarchy of Danvy and
Filinski (1990): numbered operators, one pair per level.

```text
reset2 {                       -- the List layer
  reset1 {                     -- the Option layer
    x = shift2 (k => List(1,2,3).flatMap(k))
    y = shift1 (k => lookup(x).flatMap(k))
    x + y
  }
}
```

It is correct and it is heavy: a layer is addressed by its POSITION, so
adding a layer in the middle renumbers everything above it, and a
library cannot hand out "the Option layer" as a value. This library did
not build the hierarchy (specs/shift0-dollar.md, stage 4, declined): the
next section is why it did not need to.

## Biernacki: a delimiter per monad

Marek Materzok and Dariusz Biernacki (ICFP 2011, APLAS 2012) rebuilt
delimited control around two different primitives:

- **`v $ e`** — run `e` under its own delimiter, and when it returns
  `x`, leave the delimiter and continue with `v(x)`. A plain `reset` is
  just `$` with the identity as `v`.
- **`shift0`** — capture the continuation up to a delimiter AND remove
  exactly that one delimiter.

In their calculus the CPS hierarchy is not a separate feature; it
falls out of `$` and `shift0`. And Filinski's two operators become two
lines. This library's `Layered` is exactly those two lines, on the
multi-prompt `Delim` machine (chapter 10), where every delimiter has a
NAME instead of a number:

```scala
Delim.dollar[R, M[R], F](p)(r => okay.pure(L.pure(r)))(body(using new Reflect[M, R](p, L)))
```

is `reify`: a fresh delimiter whose way out is the monad's `pure`
(`[e] = η $ e`). And

```scala
def reflect[R, F[+_]](using r: Reflect[M, R], at: At): X ! Delim + F =
  Delim.shift0[M[R], X, F](r.prompt)(k => r.layer.bind(m)(k))
```

is `reflect`: capture up to THIS layer's delimiter — past any inner
ones — and hand `k` to the layer's bind.

### 2. The basket with layered reflection

The monads stay what they are: `List`, `Either`, and a Writer that is
nothing but a case class of the user's. The one thing a monad has to
say to become a layer is how to bind a continuation — for the log,
"run the rest, then put my lines in front of its lines":

```scala
final case class Logged[A](log: Vector[String], value: A)

given Layer[Logged] with
  def pure[A](a: A): Logged[A] = Logged(Vector.empty, a)
  def bind[A, B, G[+_]](m: Logged[A])(k: A => Logged[B] ! G): Logged[B] ! G =
    k(m.value).map(next => Logged(m.log ++ next.log, next.value))
```

The program: three `reify` blocks, one per monad, outermost first, and
a body that reflects plain values of each — no transformer, no lift:

```scala
reify[List, Out, Pure]:
  reify[Logged, Either[String, Int], Pure]:
    reify[Checked, Int, Pure]:
      for
        shop  <- List("north", "south").reflect[Out, Pure]
        _     <- Logged(Vector(s"shop $shop"), ()).reflect[Either[String, Int], Pure]
        ps    <- items.foldLeft(pure[Delim + Pure, List[Int]](Nil))((acc, item) =>
                   acc.flatMap(xs => price(shop, item).reflect[Int, Pure].map(xs :+ _)))
        total  = ps.sum
        _     <- Logged(Vector(s"total $total"), ()).reflect[Either[String, Int], Pure]
      yield total
```

It gives `expected`. How it runs: `List(…).reflect` captures the rest of
the program up to the OUTER delimiter — passing through the two inner
ones — and hands it to `List`'s bind, which runs it once per shop. Each
run re-installs the inner delimiters, so each shop gets a fresh log
layer and a fresh error layer. A missing price reflects a `Left` into
the INNER layer, whose bind drops the rest of that branch; the log
layer around it still returns what that branch had written.

**The order is the nesting.** "One missing price fails the whole
basket" is the same body under the blocks in another order:

```scala
reify[Checked, List[Logged[Int]], Pure]:
  reify[List, Logged[Int], Pure]:
    reify[Logged, Int, Pure]:
```

and the answer is `Left("south has no cake")`. No new class, no new
lifts: the monads never learned about each other.

**Where did the swap go?** It moved into each layer's `bind`, and is
written against a PROGRAM (`k` returns `Logged[B] ! G`, a computation of
whatever runs outside) instead of against a named inner monad. That is
why it is written once per monad, where a transformer is written once
per monad and position.

**Why `$` and not `reset` plus a `map`.** The way out (`pure` here)
travels WITH the continuation `shift0` captures: the `List` layer calls
`k` twice, so the inner layers' ways out run twice, once per shop; a
`Left` drops `k`, so they do not run for it. `push(e).flatMap(pure)`
would apply them once, after everything — a different answer as soon as
a layer does not resume exactly once. Chapter 11 shows it with numbers
("A fifth word: `dollar`").

In a `direct` block the marks find each layer by the value's type, so
a small two-layer program reads like plain code
(`okay-direct/src/test/scala/TestBookTwoMonads.scala`):

```scala
val marked = reify[List, Option[Int], Pure]:
  reify[Option, Int, Pure]:
    direct:
      val x = List(1, 2, 3).?
      val y = lookup(x).?
      x + y
```

## Algebraic effects: operations here, meaning there

### 3. The basket with algebraic effects

The third way does not start from monads at all. The program performs
OPERATIONS — `choose`, `tell`, `raise` — that say what it wants and not
how it is done. The row lists them:

```scala
type Basket = Choose + Writer % String + Throws % String
```

```scala
def price(shop: String, item: String): Int ! Basket =
  prices(shop).get(item) match
    case Some(p) => pure[Basket, Int](p)
    case None    => raise[String, Int](s"$shop has no $item").at[Basket]

def basket(items: List[String]): Int ! Basket =
  for
    shop  <- choose("north", "south").at[Basket]
    _     <- Writer.tell(s"shop $shop").at[Basket]
    ps    <- items.foldLeft(pure[Basket, List[Int]](Nil))((acc, item) => acc.flatMap(xs => price(shop, item).map(xs :+ _)))
    total  = ps.sum
    _     <- Writer.tell(s"total $total").at[Basket]
  yield total
```

No lifts (`.at[Basket]` widens an operation into the row; it knows no
positions), and no layers anywhere in the program. The MEANING comes
from HANDLERS, applied where the program runs, one effect at a time,
innermost first:

```scala
val checked = runEither[Int, Choose + Writer % String, String](basket(items))
val logged  = Writer.collect[String, Either[String, Int], Choose](checked)
!.run(runChoice[(Vector[String], Either[String, Int]), Pure](logged)).toList
```

It gives `expected`. `runChoice` is a multi-shot handler: for `choose`
it calls the rest of the program once per shop (chapter 13), so the log
and the error handled inside it are handled per shop.

**The order is chosen where it runs.** "One missing price fails the
whole basket" is the SAME `basket`, untouched, under the handlers in
another order:

```scala
val logged  = Writer.collect[String, Int, Choose + Throws % String](basket(items))
val chosen  = runChoice[(Vector[String], Int), Throws % String](logged)
!.run(runEither[Seq[(Vector[String], Int)], Pure, String](chosen))
```

`Left("south has no cake")`. With a basket both shops can fill
(`List("tea")`) the same run is a `Right` with both branches and their
logs.

## What each one means, and the difference

The three give the same answer, and they are three different ideas of
what "several effects" is.

- **Transformers say: an effect is a monad, and several effects are a
  monad built from monads.** The composite is a TYPE; the order is in
  it; each operation is written for its position (the lifts). Honest
  and explicit, and every decision is frozen at the point of
  definition.
- **Layered reflection says: keep each monad as it is, and give each
  its own delimiter.** The monads never learn about each other; the
  program body reflects plain values; the order is the NESTING of the
  blocks around the body. It is the answer for monads a library does
  not own — the user's `Either`, a parser, a distribution — because all
  a monad needs is a `bind`.
- **Algebraic effects say: an effect is a set of operations, and its
  meaning is given later.** The program is written once against an
  interface (the row); handlers interpret it; the order is chosen where
  it RUNS, and the same program can be run under several orders — and
  under test handlers — without being touched.

**And the last two are one mechanism.** A deep handler IS a delimiter
with a way out — its return clause is `v` in `v $ e` — and performing
an operation IS a `shift0` to it (Piróg, Polesiuk and Sieczkowski,
FSCD 2019). This library runs that correspondence as a test: a `State`
handler written as `$` plus `shift0` is indistinguishable from
`State.handle` (chapter 11). So layered reflection is algebraic effects
where the "handler" is a monad's `bind`, and algebraic effects are
layered reflection where the layer is written as clauses. The library
keeps its handlers as loops because they are measured 3.8x faster than
the `$` encoding, and keeps `Layered` for the monads it does not own.

## Side by side

| | the composite is | an operation is written | the order is chosen | same program, other order |
|---|---|---|---|---|
| by hand | nested `match` | inline, every time | by the nesting you wrote | rewrite it |
| transformers (cats) | a type (`App`) | for its position, with lifts | in the type | a new `App`, every helper rewritten; helpers of another stack need lifts or a lossy hand conversion |
| Filinski 1994 | one monad (the transformer stack) | `reflect` into the stack | in the stack's type | as for transformers |
| Filinski 1999 | numbered levels | `shift_n` by position | by level number | renumber |
| layered (Biernacki) | nested `reify` blocks | `reflect` of a plain value | by which block is outside | swap the blocks |
| algebraic effects | a row of operations | the operation, no lift | where the program runs | swap the handlers |

## What it does not do

- **A layer's bind runs a program**, so a monad that cannot run a
  program inside its `flatMap` — a `Future`, whose callbacks are
  already on another thread — has no `Layer`. The capabilities paper
  (Brachthäuser, Boruch-Gruszecki and Odersky, 2020) reaches `Future` by
  running on fibres, and pays by being one-shot and JVM-only; this
  library's continuations are multi-shot on all three platforms.
- **The row must hold `Delim`**, and something must run it
  (`Delim.run`). The layers are ordinary programs in that row.
- **A capability that escapes its `reify`** fails with `NoPrompt` when
  used. `Layered.Stacked` refuses it at compile time instead
  (docs/direct-style.md, Layer 1½).
- **Not measured against transformers or against each other on this
  program.** The one measurement is the
  choice between the two spellings of `reify`: `η $ e` is 0.90 of
  `push(e.map(η))` in time and 40 B lighter per resumption
  (specs/layered-reflection.md).

## Literature

- Mark P. Jones, Luc Duponcheel, *Composing Monads* (Yale RR-1004,
  1993) — why the swap is the missing piece.
- Sheng Liang, Paul Hudak, Mark Jones, *Monad Transformers and Modular
  Interpreters* (POPL 1995) — the transformer road.
- Olivier Danvy, Andrzej Filinski, *Abstracting Control* (LFP 1990) —
  the CPS hierarchy.
- Andrzej Filinski, *Representing Monads* (POPL 1994) and
  *Representing Layered Monads* (POPL 1999).
- Marek Materzok, Dariusz Biernacki, *Subtyping Delimited
  Continuations* (ICFP 2011) and *A Dynamic Interpretation of the CPS
  Hierarchy* (APLAS 2012) — `shift0` and `$`.
- Jonathan Immanuel Brachthäuser, Aleksander Boruch-Gruszecki, Martin
  Odersky, *Representing Monads with Capabilities* (2020) —
  multi-prompt control is enough for layers.
- Gordon Plotkin, Matija Pretnar, *Handlers of Algebraic Effects* (ESOP
  2009) — operations, and their meaning given by handlers.
- Maciej Piróg, Piotr Polesiuk, Filip Sieczkowski, *Typed Equivalence
  of Effect Handlers and Delimited Control* (FSCD 2019) — a deep handler
  is `$` plus `shift0`.

---

← [16 · Continuations and monads](16-continuations-and-monads.md) ·
[Contents](index.md) ·
[17 · In the effect system →](17-in-the-effect-system.md)
