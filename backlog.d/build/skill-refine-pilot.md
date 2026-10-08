- [ ] skill-refine-pilot — okay is the pilot for rozum's `skill-refine`
      (rozum BACKLOG, "Skill refinement from agent traces"; operator,
      2026-10-08): improve the skills in `.agents/plugins` from what the
      agents working here actually did, after SkillRefiner (Khatry et al.,
      "Offline Skill Refinement from Historical Agent Traces",
      https://www.alphaxiv.org/abs/2610.skillrefiner-offline-skill-refinement):
      summarize traces, cluster successes and failures apart, one edit per
      cluster, failure-derived edits checked against the traces, merged into
      a diff of the skill. No replays. WHAT OKAY OWNS is the OUTCOME LABELS,
      read from what the repo already records, no LLM: a lane landed
      (`release-claim: … landed as <sha>`), abandoned (a claim with no
      landing and a stale heartbeat, a worktree with uncommitted work), or
      reverted; ci-runner verdicts (`.work/ci/log/`, `.work/ci/flakes/`);
      `BUGS.md`. Traces: Claude Code sessions
      (`~/.claude/projects/-Users-sergiy-work-my-okay*`, 79 for okay alone)
      and codex (`~/.codex/sessions`, 1707 across repos), filtered to okay.
      FIRST SKILL: `multi-agent`. The evidence it should learn from is
      fresh: on 2026-10-05 codex claimed three lanes and stopped —
      build-platform-processes with 9 files uncommitted in its worktree,
      semantic-core committed but never gated or landed with its heartbeat
      still at "reviewed architecture", audit-domain-close claimed with no
      change at all — and the `handler-single-pass-rest` claim had been
      idle since 09:06 that day; the same night ci-runner left master red
      (TestLoadStress, a bisect over 14 landings named no commit) with 52
      commits unpushed and nobody picked it up (a `ci-staged` cluster).
      DONE WHEN: one run over two weeks of okay's traces gives a diff to
      `multi-agent.md`, every edit citing the traces behind it, and the
      operator has accepted or rejected each edit as a PR to the
      agent-plugins submodule. A failure-derived edit is a hypothesis: where
      it can be, check it with `replay` on a recorded run before accepting.
      THE ALTERNATIVE, decided on the rozum side, not here: the pipeline on
      okay's own okay-llm/okay-rag/okay-agent — dogfooding, but rebuilding
      the embeddings, the summary cache and the MCP door rozum already has.
