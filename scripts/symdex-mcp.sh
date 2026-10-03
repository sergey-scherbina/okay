#!/bin/sh
# symdex serving THIS checkout over MCP (.mcp.json's "symdex"): definitions, references and
# callers, implementations, the given a call site resolved, a file's outline, one definition's
# source — answered from the compiler's own SemanticDB and TASTy (AGENTS.md, "Skills").
#
# The release comes from SYMDEX_HOME (a checkout or an unpacked release holding bin/symdex),
# else ~/.symdex/<version>, downloaded from GitHub once. Everything but the protocol goes to
# stderr: stdout is the MCP wire. The root is the checkout this script sits in, so a session
# in a worktree is answered from that worktree's own build.
set -e
version=0.5.0
root=$(cd "$(dirname "$0")/.." && pwd)
home=${SYMDEX_HOME:-$HOME/.symdex/$version/symdex}
if [ ! -x "$home/bin/symdex" ]; then
  dir="$HOME/.symdex/$version"
  mkdir -p "$dir"
  zip="$dir/symdex-$version.zip"
  echo "symdex-mcp: downloading symdex $version into $dir" >&2
  curl -fsSL "https://github.com/sergey-scherbina/symdex/releases/download/v$version/symdex-$version.zip" -o "$zip"
  unzip -qo "$zip" -d "$dir" >&2
  rm -f "$zip"
  chmod +x "$dir/symdex/bin/"*
  home="$dir/symdex"
fi
# a narrowed, lean server: each tool's schema is context paid on every turn (symdex GUIDE.md §4)
exec "$home/bin/symdex" serve --root "$root" \
  --tools definition,references,implementations,givens,members,outline,source,status --lean
