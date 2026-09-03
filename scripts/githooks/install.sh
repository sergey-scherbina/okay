#!/usr/bin/env sh
# Point this clone's hooks at the tracked ones. Run once per clone:
#
#     sh scripts/githooks/install.sh
#
# Tracked rather than left in .git/hooks so the rule is reviewable and survives a
# fresh clone. Worktrees share the setting, and the hooks account for that (they
# index the main checkout, never a worktree's tree).
set -eu
root=$(git rev-parse --show-toplevel)
git config core.hooksPath scripts/githooks
echo "hooks: core.hooksPath -> scripts/githooks (in $root)"
echo "they re-index this repo for rag.search after a merge/commit/checkout, and never fail one"
