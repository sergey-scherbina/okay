## direct-reset — reset and shift without `direct`, both styles in one door

`Direct.reset` and `Direct.shift` (okay-direct) take the body in either style:
a direct-style block (`.?`, `!`, or no marks under `implicitConversions`) or a
program (a `for`, a `flatMap`, a hand-written `direct { … }`), so
`reset(direct { … shift(k => direct { … }) … })` is now
`Direct.reset { … Direct.shift(k => …) … }`. The body is expected as
`R | R ! G` and a macro picks the style by its type at compile time — a
program is flattened, one extra bind. They overload every form of the core's
`reset`/`shift`, so with `import okay.Direct.*` they stand for them in that
file and the existing code compiles unchanged (the operator's choice over a
qualified-only name, which a wildcard import cannot honour). TestDirectReset;
docs/direct-style.md "reset and shift without direct". specs/direct-reset.md.
