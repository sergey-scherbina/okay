## okay-freer: one package, `okay`

Refactor, lane freer-package (specs/freer-min.md, stage 34). `Freer` goes
back to package `okay` inside okay-freer, as everything else of the module
(the CPS `Cont`, `Delimited`, the type classes) and of okay-cont's
neighbour already is: a module is a layer, not a namespace. The core's
alias (`type Freer`, `val Freer` in Free.scala) and the `import
okay.freer.Freer` lines in its files go; okay-direct's macros name
`okay.Freer` again; `Freer[?, ?, ?, ?]` is written plainly.
