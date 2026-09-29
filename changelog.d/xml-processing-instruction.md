## xml-processing-instruction - the XML dialect reads `<?…?>` and `<!DOCTYPE …>` as one token each, not an unclosed tag

- `Xml.K.Pi` (`<?xml version="1.0"?>` and any processing instruction, its
  own scanner mode: a PI ends at `?>` and nowhere else, so quotes and `>`
  inside it are its own business; the closing `?` is not the opening one)
  and `Xml.K.Decl` (`<!DOCTYPE …>`, any `<!…>` that is not a comment or
  CDATA). Neither opens a frame. Before this every document beginning with
  the XML declaration — every FpML file — ended in an `unclosed` error
  node, found by okay-refine's `Format.detect` (2026-09-29).
- Both scanners in step: `Xml.scan` and the streaming `Xml.tokens`; the
  random-input oracle (TestXmlTokens) caught the one divergence (`<?>`).
  okay2's `okay2.codec.Xml` ported the same day. TestXml: declaration, PI
  with `>` inside, DOCTYPE, unterminated PI at EOF still a token.
- okay-refine: the pinned KNOWN GAP test flipped — a declared XML document
  is `text/xml` and writes back; specs/refine.md's box closed.
