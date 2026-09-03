# The theory of Okay

*A small library built out of large ideas — and the ideas, with the
people who had them.*

---

### Part I · The foundation

**[1 · Monads and functors](01-monads.md)**
Why a computation is not a value, and what the laws buy.
*Moggi 1989/1991 · Wadler 1992/1995*

**[2 · Continuations and delimited control](02-continuations.md)**
shift, reset, prompts — and the theorem that justifies the whole tower.
*Felleisen 1988 · Danvy & Filinski 1990 · Filinski 1994 · Dybvig, Peyton Jones & Sabry 2007*

**[3 · Parameterised monads](03-parameterised.md)**
Computations that move an index: answer types, typestate, and the
diagonal back to ordinary monads.
*Atkey 2009 · Asai & Kameyama 2007*

### Part II · The machinery

**[4 · Free and freer](04-free-freer.md)**
Programs as data, signatures without constraints, three answers to the
left-nested-bind problem — and the measurement showing which one this
library needed.
*Swierstra 2008 · Kiselyov & Ishii 2015 · Voigtländer 2008 · van der Ploeg & Kiselyov 2014*

**[5 · Algebraic effects and handlers](05-effects-handlers.md)**
Operations first, meaning elsewhere; rows as unions; three shapes of
handler on one line.
*Plotkin & Power 2003 · Plotkin & Pretnar 2009 · Kiselyov, Sabry & Swords 2013*

### Part III · Making it fast, making it search

**[6 · Final tagless and staging](06-tagless-staging.md)**
Two classical answers to interpretive overhead, and Okay's three
staging rungs.
*Carette, Kiselyov & Shan 2009 · Taha & Sheard 1997/2000*

**[7 · Logic, streams and sketches](07-logic-streams.md)**
One primitive for fair search; streams as codata; approximation with
stated error.
*Kiselyov, Shan, Friedman & Sabry 2005 · Wadler 1985 · Flajolet et al. 2007 · Cormode & Muthukrishnan 2005 · Dunning & Ertl 2019*

### Part IV · The surface

**[8 · Direct style](08-direct-style.md)**
Reflection makes monads read as plain code; elaboration removes the
rest; capabilities gate the colorless version — and why multi-shot
survives here and not on Loom.
*Filinski 1994/1999 · Kameyama & Hasegawa 2003 · Flanagan et al. 1993 · Brachthäuser et al. 2020 · Lindley, McBride & McLaughlin 2017*

**[9 · Conditions: resumable exceptions](09-conditions.md)**
Signal without unwinding, restarts as prompts, the policy at the
boundary — and the direct reading where a signal is a call that may
return.
*Goldberg & Robson 1983 · Steele 1990 · Pitman 2001 · Plotkin & Pretnar 2009/2013 · Zhang, Salvaneschi & Myers 2020*

---

The [map](README.md) explains the notation (`A ! F`, `%`, `+`, `/>`),
the reading order, and the contract every chapter keeps: every claim
about Okay carries a `file:line`, every theory claim a citation, every
*why* an argument — from the papers, or from a measurement this
repository keeps.
