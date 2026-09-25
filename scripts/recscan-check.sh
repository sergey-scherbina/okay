#!/bin/sh
# recscan-check.sh [--since <ref> | --all] [--write]
#
# THE STACK-RECURSION INVENTORY ONLY SHRINKS (stack-safety stage 9,
# specs/stack-safety.md). specs/stack-safety-okay.tsv (okay2:
# stack-safety-okay2.tsv) lists every method `recscan.py` finds still
# recursing on the native stack. It is the `docs/snippet-debt.txt`
# discipline for "NO UNBOUNDED STACK RECURSION" (AGENTS.md):
#
#   NEW    a recursion recscan finds and the inventory does not name
#          (by file and def — line numbers move) is refused: make it
#          tail (@tailrec), trampolined, or write its bound as a row
#          whose sixth column says why (BOUNDED: …, LAZY: …).
#   BARE   a row this diff ADDED with no reason in its sixth column is
#          refused: a row is a written bound, not a place to park one.
#   PAID   a row whose recursion recscan no longer finds must be
#          deleted, so the file stays a true list — `--write` deletes
#          exactly those, and never adds a line.
#
# Run from a build root (the repo, or okay2/): gate.sh calls it after a
# GREEN with `--since master`, which scans only the modules whose MAIN
# sources the diff touched (a few seconds each; the whole tree is about
# a minute, `--all`). A module whose classes are older than its newest
# main source was not compiled by this gate, so it is skipped and said
# so — stale classes would report rows for code that is gone.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
since=master
mode=since
write=0
while [ $# -gt 0 ]; do
  case "$1" in
    --since) since="$2"; shift 2 ;;
    --all) mode=all; shift ;;
    --write) write=1; shift ;;
    *) echo "usage: recscan-check.sh [--since <ref> | --all] [--write]" >&2; exit 2 ;;
  esac
done
root="$PWD"
if [ "$(basename "$root")" = okay2 ]; then
  inv="$root/../specs/stack-safety-okay2.tsv"; invrel=specs/stack-safety-okay2.tsv; skip=""
else
  inv="$root/specs/stack-safety-okay.tsv"; invrel=specs/stack-safety-okay.tsv; skip="okay2"
fi
[ -f "$inv" ] || { echo "recscan: no inventory at $inv"; exit 2; }
base=""
if [ "$mode" = since ]; then
  base=$(git merge-base "$since" HEAD 2>/dev/null) || { echo "recscan: no merge-base with $since"; exit 2; }
fi
tmp="${TMPDIR:-/tmp}/recscan-check.$$"
mkdir -p "$tmp"
trap 'rm -rf "$tmp"' EXIT INT TERM
[ -n "$base" ] && git show "$base:$invrel" > "$tmp/base.tsv" 2>/dev/null || : > "$tmp/base.tsv"

RC_ROOT="$root" RC_MODE="$mode" RC_BASE="$base" RC_INV="$inv" RC_SKIP="$skip" RC_WRITE="$write" \
RC_HERE="$here" RC_TMP="$tmp" python3 - <<'EOF'
import os, re, subprocess, sys
root = os.environ['RC_ROOT']; mode = os.environ['RC_MODE']; base = os.environ['RC_BASE']
inv = os.environ['RC_INV']; skip = os.environ['RC_SKIP']; write = os.environ['RC_WRITE'] == '1'
here = os.environ['RC_HERE']; tmp = os.environ['RC_TMP']

def rows(path):
    out = []
    for l in open(path).read().split('\n'):
        if not l or l.startswith('#'): continue
        f = l.split('\t')
        out.append((f, l))
    return out
cur = rows(inv)
basekeys = {(f[0], f[2]) for f, _ in rows(os.path.join(tmp, 'base.tsv'))}
keys = {(f[0], f[2]) for f, _ in cur}

def module_of_src(p):
    """the module as recscan names it: a crossProject's platform dir
    (okay-x/jvm, okay-x/.js, …) is the module okay-x"""
    m = re.match(r'^(.*?)/?src/main/', p)
    if not m: return None
    return re.sub(r'(^|/)\.?(jvm|js|native)$', '', m.group(1)) or '.'

def newest(paths):
    t = 0
    for p in paths:
        for d, _, fs in os.walk(p):
            for f in fs:
                try: t = max(t, os.path.getmtime(os.path.join(d, f)))
                except OSError: pass
    return t

# a row this diff added must give its reason — checked first, since it
# needs no classes and holds even when no Scala source changed
bare = sorted(f[0] + '\t' + f[2] for f, _ in cur if base and (f[0], f[2]) not in basekeys and (len(f) < 6 or not f[5].strip()))
if bare:
    print(f'recscan: RED — {len(bare)} row(s) added with no reason in the sixth column (BOUNDED: …, LAZY: …):')
    for b in bare: print('  ' + b)

# a row whose FILE is gone is paid whatever was compiled: deleting a
# source compiles nothing, so no class says so
gone = [l for f, l in cur if not os.path.exists(os.path.join(root, f[0]))]
cur = [(f, l) for f, l in cur if os.path.exists(os.path.join(root, f[0]))]

def report_gone_only():
    if not gone: return False
    if write:
        drop = set(gone)
        text = open(inv).read().split('\n')
        open(inv, 'w').write('\n'.join(l for l in text if l not in drop))
        print(f'recscan: deleted {len(gone)} row(s) whose file is gone from {os.path.relpath(inv, root)}')
        return False
    print(f'recscan: RED — {len(gone)} row(s) whose file is gone; delete them (recscan-check.sh --write does, and only that):')
    for l in gone: print('  ' + '\t'.join(l.split('\t')[:4]))
    return True

# `--all` is every module with main sources; `--since` the ones the diff
# touched. EITHER WAY each must have classes newer than its sources: an
# uncompiled module has no hits, and reading "no hits" as "paid" would
# delete its rows (a first cut did exactly that, 56 rows, on an okay2
# checkout nobody had compiled)
if mode == 'all':
    changed = subprocess.run(['git', 'ls-files', '--', '*.scala'], capture_output=True, text=True, cwd=root).stdout.split()
else:
    changed = subprocess.run(['git', 'diff', '--name-only', '--relative', base], capture_output=True, text=True, cwd=root).stdout.split()
    changed += subprocess.run(['git', 'ls-files', '--others', '--exclude-standard'], capture_output=True, text=True, cwd=root).stdout.split()
if True:
    mods = sorted({m for m in (module_of_src(p) for p in changed if p.endswith('.scala')) if m is not None
                   and not (skip and (m == skip or m.startswith(skip + '/')))})
    if not mods:
        print('recscan: no main Scala source ' + ('in this build' if mode == 'all' else 'changed since the base') + ' — nothing to scan')
    fresh = []
    for m in mods:
        mdir = os.path.join(root, m)
        srcs = [os.path.join(mdir, s) for s in ('src/main',) if os.path.isdir(os.path.join(mdir, s))]
        # the module's own shared + platform sources: <m>/src/main and <m>/{jvm,.jvm}/src/main
        srcs += [os.path.join(mdir, p, 'src/main') for p in ('jvm', '.jvm') if os.path.isdir(os.path.join(mdir, p, 'src/main'))]
        cls = []
        for t in ('target', '.jvm/target', 'jvm/target'):
            td = os.path.join(mdir, t)
            if os.path.isdir(td):
                vs = [os.path.join(td, v, 'classes') for v in os.listdir(td) if v.startswith('scala-') and os.path.isdir(os.path.join(td, v, 'classes'))]
                cls += vs
        src_t = 0
        for s in srcs:
            for d, _, fs in os.walk(s):
                for f in fs:
                    if f.endswith('.scala'): src_t = max(src_t, os.path.getmtime(os.path.join(d, f)))
        if not cls or newest(cls) < src_t:
            print(f'recscan: {m}: not compiled since its last source change — skipped (compile it to have it checked)')
        else:
            fresh.append(m)
    mods = fresh
    if not mods: sys.exit(1 if report_gone_only() or bare else 0)

env = dict(os.environ)
if mods is not None: env['RECSCAN_ONLY'] = ' '.join(mods)
args = [sys.executable, os.path.join(here, 'recscan.py'), root] + ([skip] if skip else [])
found = subprocess.run(args, capture_output=True, text=True, env=env, cwd=root)
if found.returncode != 0:
    print('recscan: recscan.py failed:\n' + found.stderr[-2000:]); sys.exit(2)
hits = {}
for l in found.stdout.split('\n'):
    if not l: continue
    f = l.split('\t')
    hits[(f[0], f[2])] = l

def in_scope(path):
    return mods is None or (module_of_src(path) in mods)

new = sorted(k for k in hits if k not in keys)
paid = [l for f, l in cur if in_scope(f[0]) and (f[0], f[2]) not in hits]
paid += gone
bad = bool(bare)
scope = 'the whole build' if mods is None else ', '.join(mods)
if new:
    bad = True
    print(f'recscan: RED — {len(new)} stack recursion(s) the inventory does not name (make each tail, trampolined, or a row with its bound):')
    for k in new: print('  ' + hits[k])
if paid:
    if write:
        drop = set(paid)
        text = open(inv).read().split('\n')
        open(inv, 'w').write('\n'.join(l for l in text if l not in drop))
        print(f'recscan: deleted {len(paid)} paid row(s) from {os.path.relpath(inv, root)}')
    else:
        bad = True
        print(f'recscan: RED — {len(paid)} row(s) whose recursion is gone; delete them (recscan-check.sh --write does, and only that):')
        for l in paid: print('  ' + '\t'.join(l.split('\t')[:4]))
if not bad:
    print(f'recscan: inventory holds ({len(hits)} recursion(s) in {scope}, every one named)')
sys.exit(1 if bad else 0)
EOF
