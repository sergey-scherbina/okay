- [ ] readme-relative-links — four module READMEs link to files that do
      not exist, found by a link check run for scala2-dir (2026-09-23),
      identical on master before that lane: `okay-demo/README.md ->
      ../building-a-chat-app.md`, `okay-intent/README.md ->
      okay-agent.md`, `okay-r/README.md -> okay-py.md`,
      `okay-script/README.md -> ../okay-script-guide.md` (each meant
      `../docs/...`). Nothing in the gate checks relative markdown links;
      TestDocsIndex checks the module index only. The check that found
      them: every `](target)` outside code in docs/**/*.md and */README.md,
      resolved against its file — 871 links, these four real (five more
      hits are prose that only looks like a link). Fix the four and add
      the check to okay-deploy's doc tests.
