## the facade defined once, over the typeclass; the encoding chosen by import

Lane facade-typeclass (specs/freer-min.md, stage 48). The words a program is written in
— `A ! R`, `effect`, `op.perform`, `p.handle(h)`, `p.value`, the
row-polymorphic `Op` — are members of `Effects[M]` itself, each an alias over
the primitives, so the instance is the facade: `Effects.machine` is the
default (`import okay.*` exports it), `okay.freer.tree` the tree's, a third
encoding's is its given; the choice is the import. The gate's Native
stage runs at 14g and names the ImportSuggestions crash; the classic's
Effects.scala is Bang.scala and Classic.scala; the import-migration loop is
scripts/import-suggestions.py; 169 unused imports the runner's whole build
found are gone.
