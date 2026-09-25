- [ ] cst-walk-stack-safe — okay-parse's `Cst.lexemes` and `Cst.errors`
      recurse per nesting level (`cs.map(lexemes)`, `cs.flatMap(errors)`),
      so they overflow the JVM stack on a document `Parse.full` builds
      without trouble. Found porting to okay2 (okay2-lex-parse,
      2026-09-25): at 20 000 levels of `{"kids":[...]}` the parse passed
      and `Cst.errors` threw StackOverflowError (-Xss8m, forked);
      `lexemes` survived that depth only by a smaller frame. okay2's
      Parse.scala walks on an explicit stack (`preorder`); the same
      change here, with the depth test asserting both walks, closes it.
      TestParseDepth never saw it because it times `Parse.full` alone
      (and is Live-tagged). (2026-09-25)
