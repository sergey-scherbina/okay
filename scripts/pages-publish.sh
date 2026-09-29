#!/bin/sh
# pages-publish.sh — the release wave as a Maven repository on GitHub Pages
# (pages-maven-repo, 2026-09-29; docs/releasing.md, "GitHub Pages").
#
#   sh scripts/pages-publish.sh            # build it into the gh-pages worktree, commit, do NOT push
#   sh scripts/pages-publish.sh --push     # ...and push gh-pages
#   --snapshot                             # allow a -SNAPSHOT version (refused by default)
#
# The repository is the `maven/` folder of the gh-pages branch, served as
# https://sergey-scherbina.github.io/okay/maven once Pages is switched on for
# that branch (repository Settings, Pages). A user adds one resolver line and
# needs no token; nothing is signed and Maven Central is not involved.
#
# WHAT: the first wave (project/ReleaseWave.scala), `releaseWaveCheck` first.
# WHERE: `.work/pages/` is a worktree of gh-pages, made on the first run (an
# orphan branch when origin has none).
# A RELEASE IS NEVER OVERWRITTEN: a version without -SNAPSHOT that is already
# in the repository is refused; a SNAPSHOT is replaced, as snapshots are.
# Without --push nothing leaves this machine: read `git -C .work/pages show`.
set -eu
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/.." && pwd)"
cd "$root"

push=0
snapshot=0
for a in "$@"; do
  case "$a" in
    --push) push=1 ;;
    --snapshot) snapshot=1 ;;
    *) echo "usage: pages-publish.sh [--snapshot] [--push]" >&2; exit 2 ;;
  esac
done

site="$root/.work/pages"
repo="$site/maven"
version=$(sed -n 's/^ThisBuild \/ version := "\(.*\)"$/\1/p' build.sbt | head -1)
org=$(sed -n 's/^ThisBuild \/ organization := "\(.*\)"$/\1/p' build.sbt | head -1)
[ -n "$version" ] && [ -n "$org" ] || { echo "pages: could not read version/organization from build.sbt" >&2; exit 1; }
orgpath=$(printf '%s' "$org" | tr . /)
sha=$(git rev-parse --short HEAD)

# ---- the gh-pages worktree
if [ ! -d "$site/.git" ] && [ ! -f "$site/.git" ]; then
  git fetch -q origin gh-pages 2>/dev/null || true
  if git rev-parse -q --verify origin/gh-pages >/dev/null; then
    git worktree add -q -B gh-pages "$site" origin/gh-pages
  elif git rev-parse -q --verify gh-pages >/dev/null; then
    git worktree add -q "$site" gh-pages
  else
    # an orphan branch from an EMPTY TREE, by plumbing: `checkout --orphan`
    # plus `git rm` trips over the .agents/plugins submodule, and
    # `worktree add --orphan` needs git 2.42 (Apple's git is older)
    empty=$(git hash-object -t tree /dev/null)
    first=$(git commit-tree "$empty" -m "gh-pages: the okay Maven repository starts empty")
    git branch gh-pages "$first"
    git worktree add -q "$site" gh-pages
  fi
fi

# ---- a release stays as it was published; a snapshot only when asked
# Every publish is a commit of the whole wave, about 64 MB of jars, and a
# git branch keeps every one of them: snapshots pushed on each change would
# grow gh-pages without bound. Releases are the default; --snapshot is a
# deliberate exception.
case "$version" in
  *-SNAPSHOT)
    if [ "$snapshot" -ne 1 ]; then
      echo "pages: $version is a snapshot -- publish releases here, or pass --snapshot if you mean it" >&2
      exit 1
    fi ;;
  *) if [ -d "$repo/$orgpath/okay_3/$version" ]; then
       echo "pages: $version is already published -- a release is never overwritten; move the version on" >&2
       exit 1
     fi ;;
esac

# ---- the wave, into the worktree's maven/ folder
mkdir -p "$repo"
OKAY_PAGES_REPO="$repo" sh scripts/gate.sh "releaseWaveCheck; publish"

# ---- what Pages serves around it: no Jekyll, and a page saying how to use it
touch "$site/.nojekyll"
{
  echo '<!doctype html><meta charset="utf-8"><title>okay Maven repository</title>'
  echo '<h1>okay</h1><p>A Maven repository on GitHub Pages. In <code>build.sbt</code>:</p>'
  echo '<pre>resolvers += "okay" at "https://sergey-scherbina.github.io/okay/maven"'
  echo "libraryDependencies += \"$org\" %% \"okay\" % \"$version\"</pre>"
  echo '<h2>Artifacts</h2><ul>'
  for d in "$repo/$orgpath"/*/; do
    [ -d "$d" ] || continue
    vs=$(for v in "$d"*/; do [ -d "$v" ] && basename "$v"; done | sort | tr '\n' ' ')
    echo "<li><code>$(basename "$d")</code>: $vs</li>"
  done
  echo '</ul><p>Source: <a href="https://github.com/sergey-scherbina/okay">sergey-scherbina/okay</a></p>'
} > "$site/index.html"

# ---- one commit per publish, on gh-pages only (its own worktree)
( cd "$site"
  git add -A .
  if git diff --cached --quiet; then echo "pages: nothing changed"
  else git commit -q -m "pages: $org $version from $sha" && echo "pages: committed $version from $sha on gh-pages"; fi )

if [ "$push" -eq 1 ]; then
  git -C "$site" push -u origin gh-pages
  echo "pages: pushed. Served at https://sergey-scherbina.github.io/okay/maven once Pages is on for gh-pages."
else
  echo "pages: NOT pushed (run with --push). Look first: git -C .work/pages show --stat"
fi
