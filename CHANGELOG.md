# Changelog

## ui-screens — screens are codata, a wizard is a screen you push
Completed: 2026-09-01
Screen (view + step), Nav stack (Stay/Push/Pop/To; empty = end),
Nav.scenario fusing Dialog into a pushable screen with the answer
through the parent's continuation. 5 tests; matrix 977 (with
persist-core landed beside).

## persist-core — okay-persist: the durable log, stage 0
Completed: 2026-09-01
Spec-first (specs/persist.md — the partitioned log designed to its
distributed extent: replication with epochs, delivery semantics,
evolution, ops as values; built in stages). Stage 0: Record/Ack/
Policy/Topic/Store, FNV-1a routing, MemoryStore cross-platform,
FileStore on the JVM (versioned segment headers, CRC32C frames,
torn-tail truncation on recovery, retention by whole segments);
poll-on-end is a tested claim (the ui/mcp tailing contract). 23 JVM
+ 8 JS + 8 Native tests; full matrix green (one unrelated TestMcpHttp
flake, filed). Commits 5ffce3c..ca24db3.

## ui-wire — server-driven UI; the tree is the capability list
Completed: 2026-09-01
WireJson (hand-mapped; codec-vector filed), Wire.serve as a pure
stage (full tree, then narrow patches), Wire.client to any Host; the
forged-key test argues from the hostile side (its update THROWS on
the forged key — the wire never lets it through). 6 tests; matrix 933.

## codec-native — the P5 chain on Scala Native
Completed: 2026-09-01
okay-lex/parse/codec gain Native legs (an omission from P5, never a
decision); 57 tests pass as native binaries first try; okay-ui's Form
rides to Native. Full matrix: 927 tests.

## ui-scenarios — Dialog: a wizard is a program
Completed: 2026-09-01
Show answers an Event (a GADT); scenarios run standalone over any
Host or AS a screen inside the loop (the continuation is the state);
Form.ask/askSchema with retry-by-recursion; the demo's elicitation
loop collapsed to one line. 4 tests. Landed with spec check-off.

## ui — okay-ui v1: the toolkit that is not a toolkit
Completed: 2026-09-01
Spec-first (specs/ui.md, incl. the architecture above v1). The view
as a value (keys, not closures), diff+patch with the agreement law,
the loop over merged sources, terminal host (pure frames + stty),
React-shaped host (pure Ui=>Elem, five-line glue), Form as the fifth
Schema algebra (typed + dynamic), and MCP elicitation closed end to
end. 20 new tests. Landed: e5e19db.

## mcp — the Model Context Protocol, complete
Completed: 2026-09-01
Six tasks, spec-first (specs/mcp.md): tools/resources/prompts both
ends; duplex (subscriptions, roots, sampling as the Model effect);
transports stdio + streamable HTTP with server push over the GET
stream; acceptance against the reference server (passed first try).
Landed: 998bbc5, 955a99e, 46723fe, dd4599f, 080894e, 4a86daf.

## docs-sweep — what drifted, and what was never written down
Completed: 2026-09-01
README/ROADMAP/typepedia/tutorial corrections (Writer encoding,
groupId, counts), the upper-layers section, the MCP chapter, the
fourth kind of test, AGENTS.md. Landed: 7285974.

## stream-exercise + primitives
Completed: 2026-08-31/09-01
The fs2 exercise in okay-demo; Writer.of/map, Source + merge (bounded
by default, measured), Stage.transduce/mapAccumulate; inference fix
(one parameter list). Landed: a1f62b8..d059a9d.
