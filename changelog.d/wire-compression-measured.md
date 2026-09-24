## wire-compression-measured - DEFLATE by default on the network only, measured

The DEFLATE default (wire-deflate-default) had no number. `WireCodecBench`
(okay-py's first JMH) priced it: on a short message DEFLATE makes the
message LONGER (51 -> 53 bytes) and the round trip 4-5x slower; on a
medium one it saves 4.5x the bytes for ~9 us; on a large one 8.5x for
~0.9 ms. Bytes are the cost on a network and free on a pipe.

- `WireLink.network` (true for `WireLink.tcp`): the default preference
  compresses on a network link only. Pipes and in-process links stay
  plain unless a given asks: `WireCompression.Deflate.given` or
  `Zlib.given` still compress anywhere. `WireNegotiation.choose` takes
  `network` in place of `inProcess`. R's default is now `json/none`.
- The JDK codec pools its Deflaters and Inflaters (four per given, reset
  between messages; one that refused a message is ended): a short
  message's garbage 18.9 KB -> 2.4 KB, 4.1 -> 3.1 us. An empty message
  inflates (it was refused as cut short).
- Tests: `TestWireGivens` (pipe fake, pool), `TestGoTcp` (json/deflate
  with no import), `TestPyPipes` (json/none), `TestPyPipesDeflate`,
  `TestRWireZlib`. Mutants: the network condition, the missing reset.
- Docs: one-language.md "The wire's encoding" (the table of the
  measurement), python-and-r.md. Spec: polyglot-one-wire.md.
