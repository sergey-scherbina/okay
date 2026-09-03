#!/usr/bin/env sh
# Re-index THIS repository for `rag.search`, incrementally.
#
# The index is per project, lives at <main checkout>/.rozum/rag-index.json, and does
# NOT update itself. Without this, the tool keeps answering — confidently, out of a
# tree that no longer exists. That silent-staleness is the whole reason the hook
# exists; a search that returned nothing would at least be honest.
#
# Three rules, in order of how badly they would bite:
#   1. NEVER fail the git operation that called us. An index is a convenience; a
#      commit or a merge is not. Every path here exits 0.
#   2. Index the MAIN checkout, not a worktree. Worktrees share this hooks path, and
#      a branch's tree is not what a reader searches; master is.
#   3. Do nothing at all when `rozum` is not on PATH, so a fresh clone without it
#      commits normally.
set -u

command -v rozum >/dev/null 2>&1 || exit 0

main=$(git worktree list 2>/dev/null | head -1 | awk '{print $1}')
[ -n "${main:-}" ] && [ -d "$main" ] || exit 0

# Incremental: only files whose mtime or length moved are re-parsed. Sub-second here
# once the first full build has happened. Bounded anyway, because a hook that hangs is
# a hook that gets deleted.
if command -v timeout >/dev/null 2>&1; then
  timeout 60 rozum rag index --root "$main" >/dev/null 2>&1
else
  rozum rag index --root "$main" >/dev/null 2>&1
fi
exit 0
