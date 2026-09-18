## delim-doors-are-prompted - the ambient delimiter is evidence, not a name

`okay.ui.Scope` and `okay.llm.Cut` asked for their ambient delimiter as
a `Prompt[A] ?=>`. A `Prompt` is ONE LINE to make — `Delim.prompt[A]` —
so asking for one as a given proved nothing: a caller with no scope at
all could summon one, and `Scope.exit` or `Cut.violation` would compile
and then die at runtime with `NoPrompt`.

`Delim.Prompted`'s constructor is `private[Delim]`, so the only way to
hold one is to be inside the scope that installed it. Both ambient
doors take it now, and the same mistake is a COMPILE ERROR. The test is
a `compileErrors` of the forged program beside the real scope, because
a guard whose refusals are not asserted is not a guard.

`Scope.mark` is `Delim.scope` and `Cut.guard` is `Delim.scope` too —
which is the part worth keeping: the evidence could not be minted at
the call site even by these modules, so the move made them go through
the one door that installs a delimiter rather than assembling one out
of `prompt` + `push`. Two hand-rolled boundaries became one call each.

WHAT DID NOT CHANGE. The EXPLICIT forms — `Scope.push`/`cancel`,
`Cut.guarded`/`cut`/`checked(p, …)` — still take a `Prompt` as a
parameter, because there the prompt is the one `push` just handed you
and nothing is being proved by a type. One caller changed: the test
that BINDS an outer prompt to cross an inner scope now binds the
evidence, which is what it meant all along.

okay-llm 20, okay-chat 16, okay-ui 163.
