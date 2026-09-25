## cst-walk-stack-safe - okay-parse's Cst walks no longer overflow on deep trees

`Cst.lexemes`, `Cst.errors` and `Cst.rebase` recursed once per nesting
level (`cs.map(lexemes)`, `cs.flatMap(errors)`, `cs.map(rebase(...))`).
The builder is a fold, so `Parse.full` returned a 20 000-deep
`{"kids":[...]}` tree without trouble, and then `Cst.errors` on that
tree threw StackOverflowError. The bug was found while porting to okay2
(okay2-lex-parse). All three walks now use an explicit stack:
`lexemes`/`errors` do a pre-order over a `List`, and `rebase` rebuilds
post-order from a stack of open nodes. TestParse gained a test that
asserts all three at 20 000 levels. It failed with StackOverflowError
before the fix. TestParseDepth never caught this because it times
`Parse.full` alone and is Live-tagged. A survey found no other
recursive Cst walk: `Json.value`'s projection was already trampolined.
