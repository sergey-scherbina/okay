## okay-clojure - Clojure interop; a Stage IS a transducer, both ways

New module `okay-clojure` (JVM, Clojure 1.12.6). `Clj` calls Clojure
through its own Java API — `fn(ns, name)`, `require`, `eval` — with a
`Left` naming an unbound var, a namespace that does not load or source
that does not read. `Transducers.of(stage)` makes any okay `Stage` a
Clojure transducer (into, transduce, sequence, comp in either order);
`Transducers.stage(xf)` runs Clojure's own transducers in okay
pipelines. The translation is okay-java's `Gather` again: a tell is a
call of `rf`, a stage's answer is `reduced`, its last tells are the
completion arity.

Law-tested both ways against Clojure's `(into [] xf coll)`; early stop
from either side bounded so a broken bridge fails rather than hangs.
Four mutants: three failed their own tests at once, the fourth (the
re-run check at a step) survived until the test used a strict
transducer that errors when stepped after completion — the refusal
must come before the spent state is touched. Elements are type-tested
at the seam (a `ClassTag`) and refused by name.

Docs: docs/modules/okay-clojure.md, guide §5, theory ch. 7, index row;
every snippet in `TestDocExamplesClojure`. specs/clojure.md.
