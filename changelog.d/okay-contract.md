## okay-contract — what okay is, on one page

Operator ask, 2026-10-02. docs/contract.md names the whole contract a
user's code depends on, in three parts: `Effects[M]` as the kernel
(a monad per row, `handle`, `shift`/`reset` as an effect, `foldCont` as
the meaning), the `Applicative`/`Selective` ladder as the static half
(`Validated`, `Static`, `Par`), and the vocabulary of effects, rows and
handlers; direct style, the interop doors and every upper module are
named as built on it, not part of it. Linked first after the README's
introduction ("Start here: what okay is"), first in the README's
Start-here table and first in docs/README.md.
