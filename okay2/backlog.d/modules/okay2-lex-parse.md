- [ ] okay2-lex-parse — FIRST of the modules: okay-lex (Lex, Mealy,
      Json lexer; ~614 lines) and okay-parse (Parse, JsonParse, the
      lossless Cst; ~341) as `okay2-lex`/`okay2-parse`, cross like the
      rest. They are what the LOSSLESS readers stand on — Json's `cst`/
      `lossless`/`render` and the whole of Xml — so okay2-codec's
      lossless half and okay2-xml wait for them. Bpe.scala only if a
      consumer asks. (2026-09-25)
