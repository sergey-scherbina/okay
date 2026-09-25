## cst-walks-remaining - the last recursive CST walks now use an explicit stack

`cst-walk-stack-safe` (okay-parse) and `xml-projection-stack-safe`
(okay-codec's Xml) left four walks that still recursed once per nesting
level:
- `Yaml.values` in okay-codec;
- okay-rag's `Split.tokens`;
- `Split.structural`'s descent into a child too big for its budget;
- `Symbols.of`'s walk and the token run behind `Symbols`' `span`.

YAML nests by indentation and code by braces, so each builder makes a
tree as deep as its input. The walks then threw StackOverflowError on a
tree the builder had just made. Both new tests failed first on a 256 KB
stack:
- TestYamlDepth: a sequence nested 5 000 deep. Its first test shows
  that the builder itself takes that depth, so only the projection is
  under test.
- TestCodeDepth: 3 000 nested classes, split structurally and indexed.

The fixes:
- `values` is a post-order over two explicit stacks (tasks, results);
- `tokens` and the symbol walk are pre-orders over a list;
- the structural split keeps one frame per node being packed (its path,
  next child, pending run and output), so entering a child pushes a
  frame instead of making a call.

`recscan` over both modules no longer reports the five methods, and their
rows are gone from `specs/stack-safety-okay.tsv` (stage 2b).
