- [ ] symdex-okay — operator ask (2026-10-03): symdex
      (github.com/sergey-scherbina/symdex) serving okay. sbt-symdex in
      project/plugins.sbt turns SemanticDB on for the build (its cost on
      a clean compile measured before it lands); ci-runner's whole-build
      compile in the main checkout then keeps the index fresh after
      every landing. `.mcp.json` gains the server through
      `scripts/symdex-mcp.sh` (finds or downloads the release; a lean,
      narrowed tool set, its schema cost stated); AGENTS.md says when to
      reach for it and when grep is right, as it does for rag.search.
