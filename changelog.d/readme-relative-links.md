## readme-relative-links - every relative link in the prose resolves, and the gate checks it

- Four module READMEs pointed at files that do not exist: okay-demo
  (`../building-a-chat-app.md`), okay-intent (`okay-agent.md`), okay-r
  (`okay-py.md`), okay-script (`../okay-script-guide.md`). Each first
  paragraph had been copied from the module's docs/modules page, where
  the path was right; from the module's own directory it is `../docs/...`.
  Fixed.
- New `TestDocLinks` (okay-deploy, beside `TestDocsIndex`): every
  `](target)` in docs/, specs/, every module README (scala2/ ones too),
  README.md and ROADMAP.md must resolve. The hard part is not linking but
  deciding what IS a link in prose full of `f[A](x)`: fenced blocks (an
  info-string fence only opens), indented code and inline code are
  skipped, and a target counts only when path-shaped (a `/` or a file
  extension). 868 links; no false positive on the fixed tree; on the
  tree before, exactly the four. Control: undoing the okay-r fix turned
  the test red naming `okay-r/README.md -> okay-py.md`.
- Found by the link check run for scala2-dir; filed then, closed here.
- And a pre-commit hook for the other thing that broke every gate today:
  twice (80a1ccec, dc614d2f; fixed on master as e92ba0b1 and 516d5c4c) a
  claim copied its board item instead of `git mv`-ing it. The tracked
  `scripts/githooks/pre-commit` reads the INDEX and refuses a commit that
  files one slug twice. Tested under sh and bash on a clean tree (passes)
  and with a staged copy (refused), and by a real `git commit` of a copy:
  refused, rc 1, HEAD unchanged. AGENTS.md's Boards section says so.
