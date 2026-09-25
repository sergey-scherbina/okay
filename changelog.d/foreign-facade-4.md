## foreign-facade-4 - Programs[M]: programs as data behind the facade, one callback type over Schema values

Stage 4 of specs/foreign-facade.md. `Cb[F]` is ONE callback — a name and
a function at `Schema` types — where okay-py and okay-r each had their
own over `PyValue`/`RValue`; each `Programs` instance turns it into its
language's at the seam. `Programs[-M]` carries the language's effect as
a type member (`Op` = `ForeignEval`, `REval`), `program` answers
`Out ! (F + Op)` and `run` keeps the whole dialogue on ONE pooled worker
(that worker holds the continuations). The conformance body `programs`
runs remote-foreign's two dialogues over the facade — a callback
answered under a Reader, and a continuation resumed twice by Choice —
green over python3 here. No JVM instance: a program on the JVM is a
Scala function and nothing crosses (Decision 7). `Holds` is stage 4b.
