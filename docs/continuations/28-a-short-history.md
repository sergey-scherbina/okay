# 28 · A short history, and why each step was a narrowing

> **Part VII is where it came from.** The idea is sixty years old, and
> almost everything interesting about it is a story of **giving power
> back**. The unrestricted version was discovered first, found
> unusable at scale, and every step since has been a restriction that
> made it safer without making it weaker for the jobs people actually
> have.
>
> This chapter is last on purpose. Read first, it is trivia; read
> here, it is the reason the API in Part III looks the way it does.

---

## The shape of the story

| step | what it gave up | what that bought |
|---|---|---|
| the continuation as a semantic device | — | a way to *say* what control flow means |
| `call/cc` | — | first-class control, and a problem |
| prompts and delimiters | the whole rest of the program | a **finite** captured piece |
| `shift`/`reset` | free choice of what to re-delimit | composition, and types |
| multi-prompt | one anonymous boundary | boundaries you can **name** |
| handlers | arbitrary capture points | operations with a **signature** |
| one-shot in runtimes | resuming twice | cheap threads on real hardware |

Every row after the second is a restriction. That is the whole
argument of the chapter.

## The continuation as a way of saying what a program means

Before it was a feature it was a **description**. To explain what
"evaluate this expression" means, you say: here is the value, and here
is *the rest of the computation waiting for it*. That rest is the
continuation, and writing programs in a style where it is always
explicit — continuation-passing style — turns control flow into
ordinary function application.

Nothing here is a language feature. It is a way of talking, and it
turned out to describe `goto`, exceptions, loops, and coroutines with
one vocabulary. That is why the idea survived: it explained things
people already had.

## `call/cc`, and the discovery that made everything else necessary

Then it became a feature. `call/cc` hands you the continuation as a
function you can store, call later, call twice. With it you can build
generators, backtracking, coroutines, exceptions — none of which the
language needs to provide.

And the problem, which is the hinge of this entire history:

> **The captured continuation is the rest of the *whole program*.**

It includes your caller, your caller's caller, the web server's request
loop, the test harness. Invoking it does not "return to where you
were": it *replaces* the future of everything. A library that captures
takes hold of code it has never met and whose authors never agreed to
be re-entered.

This is not a theoretical concern. It is why `call/cc` is admired and
unused: the type of the captured thing is honest, but its *extent* is
the program, and nobody can reason about that boundary because there
is no boundary.

## Prompts and delimiters: making the piece finite

Felleisen's prompt (1988) is the fix, and it is one idea: **mark a
place, and capture only up to the mark**.

Now the captured piece is finite. It is a chunk of program with a
beginning and an end, and three things become possible that were not:

- you can give it a **type** — it takes an `A` and produces the
  delimiter's answer `R`, rather than "never returning";
- you can invoke it **more than once** without the rest of the world
  being re-entered;
- you can reason about what it touches, because "what it touches" is
  now a bounded question.

Everything in Part III of this book is downstream of that single
restriction. The library's `Prompt[R]` is Felleisen's prompt with a
type parameter and a label.

## `shift`/`reset`, and the family of four

Danvy and Filinski (1990) gave the delimited pair its working form and,
with the answer-type discipline (1989; the polymorphic account is Asai
and Kameyama, 2007), its **types**. `reset` delimits; `shift f`
captures up to the nearest `reset` and hands `k` to `f`.

Two independent choices fall out, and they generate the four operators
chapter 11 measures:

- does the handler body run **under** the delimiter?
- does the captured continuation **re-install** the delimiter when
  invoked?

`shift`, `shift0`, `control`, `control0` are the four combinations.
Chapter 11's honest finding is worth repeating here: on an ordinary
capture all four do the same thing, and it takes a second capture — in
the handler body, or inside the continuation — to separate them. The
distinctions are real and they are narrow.

And in the same decade, Filinski's theorem (1994): **given delimited
control, any monad can be embedded in direct style**. Chapter 16 is
that result, shown rather than cited. It is the reason a `direct` block
exists at all.

## Multi-prompt: boundaries you can name

One delimiter is not enough for real systems, because real systems
nest: a payment flow inside a wizard, a validator inside a generation
inside a request.

Dybvig, Peyton Jones and Sabry (2007) made the **prompt first-class** —
a tag, carried as a value, with identity. Now an inner boundary can be
crossed to reach an outer one, on purpose, by naming it.

This is the capability that nested handlers cannot express, and it is
the one chapter 26's UI scopes and chapter 25's streaming cut both
depend on. It is also where identity stops being a detail: a prompt
identified by a *name* lets two subsystems collide by agreeing on a
string (chapter 27, B3). Identity must be a fresh value; the name is
for the error message.

## The Common Lisp condition system, which got there first

Worth its own paragraph, because it is routinely left out of this
lineage and it arrived decades earlier.

Common Lisp's condition system separates **signalling** a problem from
**deciding** what to do about it, and — crucially — the handler runs
*before* the stack unwinds, so it can choose to **resume**. Restarts
are named recovery strategies offered by the code that knows the
situation, selected by the code that knows the policy.

That is the resumable-handler shape, in production, long before the
theory tidied it up. Chapter 15 is that idea rebuilt on prompts: the
signalling site offers a menu, the handler picks, and the program
continues from where it noticed the problem rather than from where
somebody caught an exception.

The lesson is not about Lisp. It is that "unwind first, then decide"
was a *choice*, made by mainstream languages, and it discards
information — the state at the point of the problem — that the
decision often needs.

## Algebraic effects and handlers: the same power, from the other side

Effects and handlers arrive at the same place from a different
starting point. Instead of "capture the rest of the program", you
declare **operations with signatures** and write handlers that
interpret them; the handler is given a continuation because that is
what interpreting an operation requires.

The two views meet in the middle. A handler is a delimiter that knows
what it catches; a capture is a handler with no signature. This book
lives at the meeting point: chapter 14 builds an effect out of a
prompt, chapter 18 says which recipes deserve to be effects, and
chapter 17 is about what happens when the two brackets are nested the
wrong way round.

The restriction that effects add is a good one: an operation has a
**type**, so a reader knows what may happen without reading the
handler.

## One-shot in modern runtimes: the restriction, chosen again

The newest chapter of the story is the oldest lesson. Loom's virtual
threads and OCaml 5's effects both give continuations that can be
resumed **once**.

That is not a compromise forced by weak implementations. It is chosen,
because a one-shot continuation can be a **stack segment** that is
moved rather than copied — which is what makes millions of concurrent
threads affordable. Multi-shot requires the continuation to be an
immutable value, and that is a different cost model.

This library goes the other way, and chapter 13 says why: the
continuation is reified as a program value, so it is multi-shot, and
chapter 24 spends that capability on forking an agent run at a tool
call to compare two futures. It also means the mechanism works on
JavaScript, where stack copying is not available at all.

Neither choice is the right one. They are different trades, and
knowing which one your runtime made is item D3 in chapter 27.

## What the history is actually telling you

Three things, and they are the design advice the rest of the book
gives in detail:

1. **Unrestricted power was tried first and abandoned.** Not because
   it was hard to implement — because nobody could reason about a
   boundary that did not exist. If your design lets a capture escape
   the thing that created it, you are back in 1990.
2. **Every restriction bought a guarantee.** Delimiters bought
   finiteness, types bought composition, prompt identity bought
   nesting, signatures bought readability. A restriction that buys
   nothing is just a restriction, and this lineage does not contain
   any.
3. **The oldest production system in the list got the hardest part
   right.** The condition system's insight — decide *before*
   unwinding — is still not in most languages, and it is the one thing
   in this book that people recognise immediately once they see it.

---

## The papers

- Matthias Felleisen. *The theory and practice of first-class
  prompts.* POPL 1988.
- Olivier Danvy, Andrzej Filinski. *A functional abstraction of typed
  contexts.* DIKU report 89/12, 1989.
- Olivier Danvy, Andrzej Filinski. *Abstracting control.* LISP and
  Functional Programming 1990.
- Andrzej Filinski. *Representing monads.* POPL 1994.
- Christian Queinnec. *The influence of browsers on evaluators, or
  continuations to program web servers.* ICFP 2000.
- Kenichi Asai, Yukiyoshi Kameyama. *Polymorphic delimited
  continuations.* APLAS 2007.
- R. Kent Dybvig, Simon Peyton Jones, Amr Sabry. *A monadic framework
  for delimited continuations.* JFP 17(6):687–730, 2007.
- Rúnar Bjarnason. *Stackless Scala with free monads.* 2012.
- Fei Wang, Tiark Rompf. *A Language and Compiler View on
  Differentiable Programming.* ICLR Workshop, 2018.

The links, and the places each is used in this implementation, are in
[docs/theory/02-continuations.md](../theory/02-continuations.md).

---

← [27 · Everything that typically goes wrong](27-what-goes-wrong.md) ·
[Contents](index.md) ·
[Appendix A · If you really want to, you can →](appendix-a-if-you-really-want.md)
