# Typestate: effects with the state's type on the tree

A protocol is an order of operations. `begin` before `commit`, `open`
before `read`, a state that is an `Int` now and a `String` after the
next step. Okay's program tree, `Freer[G, S, R, A]`, carries two type
indexes beside the value, and since 2026-09-30 they are invariant and
mean whatever the signature `G` says they mean. This page is about the
reading where they are a STATE: `R` before the operation, `S` after,
and a handler that holds a value of that type and moves it. The theory
is [theory/3](theory/03-parameterised.md); the decisions and their
numbers are `specs/freer-base.md` and `specs/indexed-effects.md`.

## Two readings of one pair

The tree knows one rule about `S` and `R`: a `Bind` joins a left side
at `(T, R)` to a continuation at `(S, T)` and is itself at `(S, R)`.
The middle type must meet. What the pair *means* is the signature's
choice, and two readings type:

- **Answer types** (`Cps`, `PState.get`/`set`): a program is `(A => S)
  => R`. `R` is what running it produces, `S` what its continuation
  must produce. The handler is Cps's runner and a shift body gets `k`.
  This is Danvy and Filinski's answer-type modification.
- **A state the handler consumes** (`PState.Threaded`, `Tx.Data`): a
  program is `R => (S, A)`. The handler holds an `R`, runs the
  operation, and continues holding what the operation gives it. No
  continuation object, no re-entry: `State.handle`'s loop with the
  type moving.

Both live on the same nodes. The second costs what the untyped
`State` costs, within 7% (HandlerBenchmark, `stateThreaded` against
`stateEffect`); the first pays a frame per operation and reads 1.79x.
Take the first for what only it can do — a body that uses `k`, the
profunctor `PState.Zooming` — and the second for a protocol.

## A typestate as data

`PState.Threaded` is the type-changing state written as an indexed
signature. Two operations, and the transitions are on the cases:

```scala
  enum Op[S, R, +X]:
    case Get[S]() extends Op[S, S, S]
    case Put[S, T](t: T) extends Op[T, S, S]
```

`Get` reads the state and leaves its type; `Put` replaces it, moving
the type from `S` to `T`, and answers the old state, as `PState.set`
does. A program threads an `Int` into a `String` into a `Boolean`, and
the compiler holds the order — a `put[String, …]` after a `get[Int]`
does not type:

```scala
    import PState.Threaded.{get, put}
    val r = PState.Threaded.run(
      for
        n <- get[Int]                          // n: Int
        _ <- put[Int, String]((n + 1).toString) // the state is a String now
        s <- get[String]                       // s: String
        old <- put[String, Boolean](s.length == 2) // the old state is the answer, as set's is
      yield s + "!" + old)(41)
    assertEquals(r, (true, "42!42"))
```

`Threaded.run` is the handler: `Return` gives it `S = R`, so the pair it
answers is the state it holds; `Get` gives its state's type to the
continuation; `Put` hands the new state on. It is `@tailrec` with the
type arguments changing per call, which Scala 3 accepts.

## A protocol beside ordinary effects: the indexed row

A protocol rarely stands alone. `Indexed.scala` is the row of indexed
signatures: `F +~ G` is `+` at three parameters, and an ordinary
effect enters it as `Unary[F]`, a member that IS `F[X]` on the
diagonal and nothing off it — so a `State` operation can only enter
through `Indexed.unary`, the door that moves no index, and
`Indexed.effect` at a moving index refuses it by the compiler:

```scala
  type Row = PSt +~ Unary[State[Int, *]]
  given TypeableI[PSt] = TypeableI.derived
  def rget[S, Z]: Freer[Row, S => Z, S => Z, S] = Indexed.effect[Row, S => Z, S => Z, S](PSt.Get())
  def rput[S, T, Z](t: T): Freer[Row, T => Z, S => Z, S] = Indexed.effect[Row, T => Z, S => Z, S](PSt.Put(t))
  def tick[R]: Freer[Row, R, R, Int] = Indexed.unary[Row, R, Int](State.Update[Int, Int](n => (n + 1, n + 1)))
```

`State.handleIndexed` is the reference handler over such a row: its
own operations answered from the threaded state, every other
operation forwarded with the index it came with. A handler for your
own indexed effect has the same shape, and `splitI` is `split` at
three parameters.

## A real protocol: the transaction, with the connection typed

okay-sql's `Tx.Data` is the transaction protocol as data. The
transitions are on the signature, said once:

```scala
  enum TxOp[S, R, +X]:       // the tree's order: `R` the state before, `S` after
    case Begin(isolation: Isolation, readOnly: Boolean) extends TxOp[Open, Idle, Granted]
    case Commit() extends TxOp[Idle, Open, Unit]
    case Rollback() extends TxOp[Idle, Open, Unit]
    case Update[S](sql: String, params: Vector[SqlValue]) extends TxOp[S, S, Long]
    case Batch[S](sql: String, rows: Chunk[Vector[SqlValue]]) extends TxOp[S, S, Long]
    case Describe[S](sql: String) extends TxOp[S, S, Vector[Col]]
```

A program over `Tx.Data[A, From, To]` (the alias reads left to right)
is checked along every `flatMap`: a nested `begin`, a `commit` with no
`begin`, a program left open — each a compile error. The body is not
the protocol alone: the row is `TxOp +~ Unary[Async]`, so `Tx.Data.
async` runs any `Async` program inside a transaction, on the diagonal.

What the data road adds is in the handler. `Tx.Data.interpret` holds
`Conn[Idle]` or `Conn[Open]`, the connection typed by the index, and
moves it only in the arm that runs the driver's own `begin`, `commit`
or `rollback`. A `commit` from an idle connection does not type even
inside the handler. Against a recording driver:

```scala
    val n = interpret(
      begin().flatMap { g =>
        update[Tx.Open]("insert into t values (1)").flatMap(_ => commit()).map(_ => g.granted)
      })(db).runWith
    assertEquals(n, Isolation.ReadCommitted)
```

One caveat the type cannot close: a failure inside the body drops the
continuation, and the `commit` with it. The index is a protocol of the
text, not a run-time guarantee; a body that may fail is bracketed by
its caller, as it always was.

## Which road, when

- The type never changes: `State % S`, the ordinary effect.
- The type changes and the program is a protocol: the data road —
  `PState.Threaded`, or your own `enum Op[S, R, +X]` with the handler
  in `Threaded.run`'s shape.
- The body needs the continuation as a value — multi-shot, an answer
  computed from `k`, a zoom through a profunctor: the shift road,
  `PState.get`/`set` over `Cps`.
- The protocol lives beside other effects: the indexed row, `Op +~
  Unary[F]`, and a handler in `State.handleIndexed`'s shape.

## Literature

- Robert Atkey. *[Parameterised notions of
  computation.](https://bentnib.org/paramnotions-jfp.html)* JFP 2009.
  The parameterised monad, its diagonal, and typestate as its example.
- Conor McBride. *[Kleisli arrows of outrageous
  fortune.](https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf)*
  2011. The indexed free monad whose index the handler consumes; here
  without its value-dependent post-state, which a sum-typed state
  encodes.
- Olivier Danvy, Andrzej Filinski. *A functional abstraction of typed
  contexts.* DIKU 89/12, 1989. Answer-type modification, the other
  reading of the same pair.
