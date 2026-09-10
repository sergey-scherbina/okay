# Your own effect

An effect in this library is a plain enum. Nothing is registered
anywhere, no framework knows about it, and the whole declaration is
the operations and what each of them answers. What follows is one
worked effect from that declaration to four interpretations of the
same program — a real SQLite file, a Map, a trace of either, and pure
`State` + `Writer` with no mutation anywhere.

Everything here is `import okay.*` away, and the file it is drawn from
runs:

```
sbt "okayJdbc/Test/runMain okay.demoeff.UsersDemo"
```

## 1. The declaration

```scala
enum Users[+A] derives Effect:
  case Find(id: Long) extends Users[Option[String]]
  case Save(id: Long, name: String) extends Users[Option[String]]
```

Each case says what it ANSWERS in its own type: a `Find` answers an
`Option[String]`, a `Save` answers the name it replaced. That is a
GADT, and it is what lets a handler be written once and typecheck per
operation.

`derives Effect` writes the one instance a row needs. A row is an
untagged union — `A ! (Users + Writer % String)` is `Free[[X] =>>
Users[X] | Writer[String, X], A]` — and unions erase, so when a
handler for `Users` meets an operation it decides by a runtime class
test. `Effect` IS that test (it extends `TypeableK`), built from a
`ClassTag`, and it also registers the signature for direct-style
auto-coloring. If you want the test without auto-coloring, write
`derives TypeableK`.

It is not optional. There is no generic fallback instance any more,
so a signature that declares nothing is a compile error at the place
it is USED, with a message naming both spellings. The fallback that
used to cover it made every effect that forgot work anyway, at a
warning per use site its author never saw.

A signature with parameters says the same thing — `derives` abstracts
the LAST type parameter:

```scala
enum Cache[K, +A] derives Effect:     // Cache % K is the row member
  case Get(k: K) extends Cache[K, Option[String]]
```

One caveat, since it is the first thing that bites: in a file that
does `import okay.!.*`, the name `Effect` is that object's alias for
an operation node. Write `derives okay.Effect` there.

## 2. Constructors, or none

```scala
object Users:
  inline def find(id: Long): Option[String] ! Users = effect(Find(id))
  inline def save(id: Long, name: String): Option[String] ! Users = effect(Save(id, name))
```

These are optional. `Users.Find(id).perform` says the same thing with
nothing declared — the answer type comes from the case, so unifying
the receiver against `F[A]` recovers both the signature and what it
answers. Write the named constructors anyway for an effect other
people will use: they are its API, they cost one line each, and every
call site reads better for them.

`perform` applies to any `F[A]`, including types nobody declared as a
signature — and that is not the hazard it looks like. A freer monad
takes any type constructor, so `List(1, 2).perform` is nondeterminism,
handled by `runSeq`, which is `runChoice`'s handler unchanged:
`Choose[+A](as: Seq[A])` is a box around exactly this.

## 3. Rows: what a program can do

A constructor builds at its OWN row. To use it beside another effect,
move it:

```scala
Users.find(id).plus[Abort]        // Option[String] ! (Users + Abort)
State.get[S].at[R]                // into a row known only by membership
```

`plus[R]` adds R to whatever row the program has. `at[R]` names the
target instead, and is for the one shape `plus` cannot express: an
ABSTRACT row, known only by the fact that it CONTAINS something — what
a row-polymorphic helper has (`[R[+_] : Has[State % Int]]`).

Both are one cast under a witness that F is a member of R, and both
measure at the floor: the same bytes per operation as constructing it
at R in the first place (`RowLiftBenchmark`). `!.widen` stays for
whole programs — its walk is also a normalisation, and deleting it
costs 5-7% on `Source.merge`.

Row ORDER does not exist. `+` is a union and `|` commutes, so
`A ! (Users + Abort)` and `A ! (Abort + Users)` are the same type and
assign both ways with no coercion. An `.at[...]` written to reorder a
row is noise.

## 4. A step the program may decline

```scala
def rename(id: Long, to: String): Option[String] ! Users = runOption {
  for
    case Some(old) <- Users.find(id).plus[Abort]
    _              <- Users.save(id, to).plus[Abort]
  yield old
}
```

A for-comprehension SEQUENCES and does not branch, so `find` then
`save` would reach the handler and store a name for an id nobody has.
The fix is the pattern plus a row that says this program MAY STOP.

`case Some(old) <-` and an `if` guard both desugar to `withFilter`, so
neither is about patterns or booleans: each asks whether a step may be
DROPPED. A plain `A ! F` may not — nothing in `Free` declines to
answer — and the witness `CanFail[F]` is carried by membership of an
effect that can:

| the row carries | a failed pattern means | the handler answers |
|---|---|---|
| `Abort` (= `Throws % Unit`) | the PROGRAM stops | `runOption`, `None` |
| `Choose` | this BRANCH dies, the search goes on | `runChoice`, the branches that matched |

Where a row carries both, `Choose` wins: in a searching row `guard`
already means prune. Where it carries neither, the compiler refuses
and the message names both ways to fix it.

Outside a for-comprehension the same demand is `ensure[R](cond)`, and
a failure is answered in the row by `p.orElse(q)` or `p.recover(h)` —
`runEither` applied to a PART of the program, so the row comes out
unchanged and what follows neither knows nor cares. Not free: one
handler per call, so wrap the smallest piece that can fail.

## 5. Handlers

A `Handler[F]` answers each operation with a value. Production talks
to SQLite in plain JDBC:

```scala
def live(c: Connection): Handler[Users] = new:
  def handle[A](e: Users[A]): A = e match
    case Users.Find(id) => selectName(c, id)
    case Users.Save(id, name) =>
      val was = selectName(c, id)
      // an UPSERT: asked about an id nobody has, this handler would
      // create the row. The program never asks, and that is the point
      // — the guarantee is in the program's type, not in the
      // handler's good manners.
      upsert(c, id, name)
      was
```

Note what the GADT bought: in the `Find` branch the compiler knows the
answer must be an `Option[String]`, and in `Save` that it must be one
too. No casts, no `Any`.

A test handler is the same shape over any store you like. Make the
store a type class rather than a `Map`, and say what a store is once:

```scala
trait Store[S]:
  def get(id: Long): S => Option[String]
  def put(id: Long, name: String): S => S

  /** replace, ANSWERING what was there — the two above in the order
   * that makes the answer true, said once here instead of at every
   * call site */
  def replace(id: Long, name: String): S => (Option[String], S) =
    s =>
      val was = get(id)(s)
      (was, put(id, name)(s))
```

`put` answers a NEW store rather than mutating this one. That
immutability is a law: the compiler does not check it, so test it —
run the whole interpretation and assert the value you passed in is
still what it was. And put the read-then-write in the contract, where
the instance author owns it: a carrier that can do the swap in one
step overrides `replace` and does not lean on the law at all.

The handler over it holds the successor in a `var`, which is the only
mutation in it — a handler must answer with a value, so something has
to remember:

```scala
final class InMemory[S](init: S)(using St: Store[S]) extends Handler[Users]:
  private var s = init
  def state: S = s
  def handle[A](e: Users[A]): A = e match
    case Users.Find(id)       => St.get(id)(s)
    case Users.Save(id, name) =>
      val (was, next) = St.replace(id, name)(s)
      s = next
      was
```

## 6. Recording is a decorator

Operations are already data, so "what did this program ask for, and in
what order" needs no second handler that can drift from the first:

```scala
rename(7, "grace").runWith(using mem.tracing(log += _))
// log == Seq(Find(7), Save(7, "grace"))
```

`h.tracing` wears any handler, the SQLite one included — recording is
not a test-only trick. `!.tracing(p)(show)` is the same idea one level
up: it records at the PROGRAM level, before anything is interpreted,
telling each operation to a `Writer` and then performing it exactly as
before. It knows nothing about the effect beyond `show`.

## 7. Interpreting into other effects

A handler must ANSWER with a value, so it cannot itself get or tell.
An interpreter can: it turns each operation into a PROGRAM in another
row, where `State` and `Writer` are ordinary members.

```scala
def stored[A, S : Store as S, F[+_]](prog: A ! (Users + F)): A ! (State % S + F) =
  !.interpret(prog):
    [X] => (e: Users[X]) => e match
      case Users.Find(id) =>
        State.get[S].plus[F].map(S.get(id))
      case Users.Save(id, name) =>
        State.update[S, X](S.replace(id, name)).plus[F]
```

`!.interpret` is `!.translate` with the widening done for you, for
when the target row is BIGGER than the source's; `F` — whatever the
caller was already doing — rides through untouched, and the expected
type solves every row, so there is no type argument to write.

`State.update(f)` is the transition that ANSWERS something the write
is about to destroy: `f` sees the state and returns what to answer and
what to leave behind. `modify` answers the new state, `swap` answers
both.

Two layers, one job each, composed:

```scala
def tracked[A, S : Store, F[+_]](prog: A ! (Users + F)): A ! (State % S + Writer % String + F) =
  stored[A, S, Writer % String + F](
    !.tracing(prog)([X] => (e: Users[X]) => e.toString))
```

`stored` answers and could not log if it wanted to — it knows nothing
about a Writer being in the row. `!.tracing` logs and answers nothing.
Order is the meaning: recording happens BEFORE interpretation, so the
log holds what the program ASKED, not what the store did about it.

Running it needs no new handler either. `tracked` left `Users` behind
and produced `State` and `Writer`, so those two are handled by their
own runners, innermost first:

```scala
def pureRun[S : Store](init: S): (S, (Seq[String], Option[String])) =
  State.run[S, (Seq[String], Option[String])](init)(
    Writer.run[String, Option[String], State % S](
      tracked[Option[String], S, Pure](rename(7L, "grace"))))

pureRun(Map(7L -> "ada"))
// (Map(7 -> grace), (Seq(Find(7), Save(7,grace)), Some(ada)))

pureRun(Vector(7L -> "ada"))
// (Vector((7,grace)), (Seq(Find(7), Save(7,grace)), Some(ada)))
```

Three values fall out — the final store, the log, and the answer — and
the carrier is a type class parameter, so swapping a `Map` for an
association list changes nothing above it: not the interpreter, not
the program.

## 8. Several instances of one effect

A row is split by a runtime test, so two members of the same signature
are told apart exactly when the operation carries something to
compare. `Tag` is that, for any signature:

```scala
type Small = Tag.Of["small", State % Int]
type Big   = Tag.Of["big",   State % Int]

// an ordinary function, written against a plain State % Int,
// run twice at two different states in one program:
val twice: (Int, Int) ! (Small + Big) =
  for
    a <- Tag.tag["small", State % Int](bump(1)).plus[Big]
    b <- Tag.tag["big",   State % Int](bump(10)).at[Small + Big]
  yield (a, b)
```

`tag` walks a FINISHED program and puts every operation of F under the
key — which is the point: `bump` did not have to be written for this.
Handling needs no new handler: `untag` strips one key and hands the
plain signature back to its own runner.

Where the instances are MADE rather than named, `Refs` is the
counterpart — cells created at run time, identity by cell, one row
member however many there are:

```scala
val p: (Int, String) ! Refs =
  for
    a <- Refs.ref(1)
    b <- Refs.ref("ada")
    _ <- Refs.write(a, 2)
    x <- Refs.read(a)
    y <- Refs.read(b)
  yield (x, y)

Refs.run(p)   // (2, ada)
```

The price is stated where it is paid: the row no longer says which
states there are, and the handler's heap is keyed by identity, so one
cast turns a slot back into its type. Use a key when the instances can
be named, cells when they are made. A third route exists for the case
where they must be nested and separated dynamically — a fresh prompt
per handler installation, which `Delim`'s multi-prompt control already
supports.

## 9. What bites

**A branch that only widens.** Matching a case declared `Users[Unit]`
inside `[X] => (e: Users[X]) => ...` proves only `X >: Unit`, never
`X = Unit`, because the signature is COVARIANT — so a branch answering
`Unit` must widen, usually as `.map(_ => ())`. Three ways out, in
increasing cost: give the operation something to answer (free, and
usually the better model); use a combinator that answers `Unit`; make
the signature invariant, which costs a cast in `TypeableK` and churn
everywhere. specs/writer-covariance.md has the measurements, including
what covariance is actually FOR: the `Typeable[F[Nothing]]` instance,
and declaring an operation that never answers once as `Tx[Nothing]`.

**One instance per signature, unless the operation carries identity.**
`State % S`'s `Get()` carries no trace of S, so a row holds ONE of it
and two at different S misroute — loudly (a ClassCastException at the
first wrong answer), which `TestRowIdentity` demonstrates. `Writer`
escapes this without trying, its operation being the value told. `Tag`
and `Refs` above are the general fixes.

**A `Handler` cannot get or tell.** If your interpretation needs other
effects, it is an interpreter (`!.interpret`), not a handler.

**Covariance is per case.** A case that declares its own type
parameter gets an exact answer type even under `+A`, and may CONSUME
that type in a parameter — `case Fold[A](seed: A, step: (A, Int) => A)
extends Op[A]` — which a case indexed at the enum's own `+A` cannot.

## The whole thing, running

`okay-jdbc/src/test/scala/okay/demoeff/UsersDemo.scala` is this page as
one file, with the SQLite handler, the in-memory one, the traced runs
and the pure interpretation over two carriers:

```
PROD   Some(ada) / row 7 is now grace
TEST   Some(ada) / log=Find(7), Save(7,grace) / state=Map(7 -> grace)
MISS   None / row 99 is now - / both worlds agree: true / log=Find(99)
DIRECT (Some(g),None)
PURE   Some(ada) / log=Find(7), Save(7,grace) / store=Map(7 -> grace)
PURE2  Some(ada) / log=Find(7), Save(7,grace) / store=Vector((7,grace))
```

`TestUsersDemo` turns those claims into assertions — the two carriers
agree, `replace` answers what was there and leaves its argument
untouched, and the handler world matches the pure one, including the
missing id: a find, no save, nothing written.
