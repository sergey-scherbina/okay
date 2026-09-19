# Bugs — okay-ui

Defects owned by this module. Status lives in the machine-readable
header, never in prose. Newest first.

## wire-close-test-hang — a Live end-to-end test hung forever, on the CLIENT side
<!-- status: fixed
     lane: wire-close-test-hang
     area: okay-ui/src/test/scala/okay/ui/TestWire.scala
     found-by: manually running the new "end to end, SERVER-initiated"
       test (added by wire-server-close) with `sbt integrationTest` —
       the process sat at 0% CPU for 2h44m; a thread dump pinned it to
       `TestWire.$anonfun$7` at `fiber.join()`
     gate: `sbt integrationTest` (scoped: `okayUiJVM/testOnly
       okay.ui.TestWire` with the class's own Live tag disabled
       locally to drive it directly), 5 repeats clean; the full
       monorepo `sbt test` afterward, 0 failures
     fixed-in: (this lane)
     confirmed: yes -->

**The gate that landed `wire-server-close` never actually ran this
test.** Its default form (`sbt test`) excludes anything tagged
`Live`, and this suite is tagged at the class level — so the new test
compiled, was described as passing in that lane's own changelog
entry, and never once executed before landing on `origin/master`,
where the NEXT person to run `sbt integrationTest` would have hung
for however long they were willing to wait.

The test ended its own event feed with `feed.close()`, on the
assumption that closing the channel `Wire.client`'s `forward` loop
reads from would end it. It does not: `forward` only ever stops on
its OWN `Event.Closed` (the hybrid rule `Wire.client` documents),
and `Writer.uncons` on a channel that is closed-but-otherwise-idle
still awaits a next item that is never coming. `fiber.join()` — a raw
thread park with no timeout of its own — then never returns.

The fix sends `Event.Closed` as the client's own last event, the same
way every other test in this file already does, and drops the
trailing press the test used to race against the server's close: `up`
and `down` are two independently scheduled fibers, so an event sent
after "logout" has no ordering guarantee against when the server acts
on it, and asserting across that race tested the scheduler rather
than `Wire.serveClosing`.

**The lesson, not just the fix:** a Live-tagged test is invisible to
the default gate in BOTH directions — a broken one and a hung one
look identical to `sbt test`, which is silence. Landing anything that
touches a Live suite needs the Live suite actually run, once, before
the claim is released; a changelog line describing what a test does
is not evidence that it ran.

## react-host-vocab — a node the host DOES claim is lowered away inside one it does not
<!-- status: fixed
     lane: ui
     area: okay-ui/src/main/scala/okay/ui/React.scala
     found-by: okay-watch's analyst page, 2026-09-18 — the transfers
       table links each transaction hash and block height into a public
       explorer, and every one of them arrived as plain text
     gate: okay-ui/src/test/scala/okay/ui/TestLink.scala — "a Link the
       browser claims survives a node it does not" and "THE TWO ROADS
       AGREE ON ONE TREE", both red before the fix
     fixed-in: 94e830b0 (spec deebb5c1)
     confirmed: yes
     repro: measured 2026-09-18
       Html.render(Ui.Table(Vector("where"), Vector(Vector(
         Ui.Link("cases .csv", "/api/cases.csv"))), "t"))
       -> <span ...>cases .csv — /api/cases.csv</span>   (wanted <a href=...>)
       Html.render(Ui.Link("cases .csv", "/api/cases.csv"))
       -> <a href="/api/cases.csv">cases .csv</a> -->

`React.elem` claims `Ui.Link` and renders an anchor. Its catch-all
asked `Ui.lower` for the meaning of every node it does NOT claim, and
passed `Set.empty`:

    case semantic => elem(Ui.lower(semantic, Set.empty))

`Ui.lower` RECURSES, so that empty vocabulary reached the CELLS. A
`Link` inside a `Table`, `Items`, `Tabs`, `Modal` or `Disclosure`
therefore came out as its lowering, `Text("label — href")`, one node
away from where the same link was an `<a>`. Nothing failed; the page
quietly stopped being clickable.

**The defect was not that the set was empty.** `Frame` and `Swing`
claim no semantic node at all, so `Set.empty` IS their vocabulary and
their identical-looking line is correct. It was that a CONSTANT stood
where a host's own vocabulary belongs — the call said "lower this
whole subtree as if I drew nothing" when it meant "lower this node".

**What it cost is the sharper statement: the two roads disagreed on
one tree.** `live.js` sends `vocab: ["link"]` in its hello, so
`Wire.serve` lowered with `link` and the socket carried the anchor,
while `Html.render` — the scriptless road through `React.elem` —
carried text. okay-watch serves both roads from one `Analyst.view`,
and a page that renders differently depending on whether a script ran
is not one page. That is now a test rather than a sentence:
`Html.render(t)` equals `Html.render(Ui.lower(t, Set(Vocab.link)))`.

Fixed by naming the set: `React.Vocabulary = Set(Vocab.link)`, the
same set `live.js` declares, passed where the constant was. The
recursion terminates because every name in it is matched above the
catch-all, so a lowered node never returns to that case.

**Why no test caught it for a day.** `TestLink` asserted the anchor on
a BARE link and the lowering on a bare link, and both were right. The
nesting was the untested case, and it is the only one a real page
produces — a link in a page lives in a table, a list or a dialog.
The three tests added with the fix are all nested.

**What it is worth remembering for.** A host's catch-all is the one
place where a whole subtree changes meaning, and `Ui.lower`'s
recursion is what makes the argument to it a claim about the subtree
rather than about the node. Any future host that claims a semantic
node must pass its own vocabulary there; a constant will be wrong the
moment that host claims anything.
