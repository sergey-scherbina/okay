#!/usr/bin/env python3
"""
compile, read the compiler, fix the imports, repeat — the loop that carried the import migrations of
classic-to-freer, effects-rows and facade-typeclass (specs/freer-min.md, stages 45-48).

    python3 scripts/import-suggestions.py "<sbt command>"     # e.g. "affected master Test/compile jvm staged"

Each round runs the command through scripts/gate.sh and reads its log:
  - "The following import might fix the problem: import okay.X" -> X is added to the file's `import okay.{...}`
  - "Not found: X" / "value X is not a member" / "Expected a type, but found a term: X" -> the same, when X is
    a top-level name of package `okay` (computed from the sources) or of `okay.freer` (added to the file's
    `import okay.freer.{...}`)
  - "Reference to X is ambiguous ... imported by name by import okay.X" -> the file hides the facade and takes
    the wildcard: `import okay.{! as _, + as _, ..., *}` (a classic file naming a same-named term by name)
  - "unused import" -> the ONE selector at the caret is removed (the line, if it was the last)
It stops when a round reports no error and no warning (CLEAN), or when it changed nothing (STUCK). It only
edits import lines; a file it cannot anchor an import in (no package clause, no import) is left alone and
named. The names it adds are the compiler's own suggestions where there are any, so a wrong addition is an
unused-import warning the next round removes, never a silent change of meaning.
"""
import re, os, subprocess, glob, sys

ROOT = os.getcwd()
CMD = sys.argv[1] if len(sys.argv) > 1 else 'affected master Test/compile jvm staged'
HIDE = 'import okay.{! as _, + as _, % as _, Pure as _, pure as _, effect as _, perform as _, handle as _, value as _, *}'
FACADE = {'!', '+', '%', 'Pure', 'pure', 'effect', 'perform', 'handle', 'value'}
DEFRE = re.compile(r'^(?:(?:infix|transparent|inline|opaque|final|abstract|sealed|implicit|private\[\w+\]|private|protected|lazy|case)\s+)*(?:type|def|val|var|object|trait|class|enum|given)\s+([A-Za-z_]\w*|[^\s(\[:]+)', re.M)

def toplevel_names(package_re, skip_dirs):
    names = set()
    for d, ds, fs in os.walk('.'):
        ds[:] = [x for x in ds if x not in ('target', '.git', 'node_modules', 'okay2') and os.path.join(d, x) not in skip_dirs]
        for f in fs:
            if not f.endswith('.scala'): continue
            s = open(os.path.join(d, f), errors='replace').read()
            if not re.search(package_re, s, re.M): continue
            lines = s.split('\n'); i = 0
            while i < len(lines):
                l = lines[i]
                m = DEFRE.match(l)
                if m: names.add(m.group(1))
                if re.match(r'^extension\b', l):
                    i += 1
                    while i < len(lines) and (lines[i].startswith('  ') or lines[i].strip() == ''):
                        m = re.match(r'^  (?:(?:inline|transparent|infix|final)\s+)*def\s+([A-Za-z_]\w*|[^\s(\[:]+)', lines[i])
                        if m: names.add(m.group(1))
                        i += 1
                else: i += 1
    return names

CORE = (toplevel_names(r'^package okay\s*$', {'./okay-cont', './okay-freer'}) | {'==>'}) - FACADE - {'C'}
FREER = toplevel_names(r'^package okay\.freer\s*$', set()) | {'Classic', 'Row', 'Rowed', 'FreeEffects'} | FACADE

def latest_log():
    import tempfile
    ls = [l for l in glob.glob(os.path.join(tempfile.gettempdir(), 'okay-gate.*')) + glob.glob('/var/folders/*/*/T/okay-gate.*') + glob.glob('/tmp/okay-gate.*') if not l.endswith('.clean') and 'platform' not in l]
    return max(ls, key=os.path.getmtime)

def tick(n): return n if re.match(r'^[A-Za-z_]\w*$', n) else '`' + n + '`'

def add_to(f, prefix, names):
    s = open(f).read()
    m = re.search(r'^(\s*)import ' + re.escape(prefix) + r'\.\{([^}]*)\}\s*$', s, re.M)
    if m:
        cur = [x.strip() for x in m.group(2).split(',') if x.strip()]
        new = cur + [tick(n) for n in names if tick(n) not in cur]
        s = s[:m.start()] + m.group(1) + 'import ' + prefix + '.{' + ', '.join(new) + '}' + s[m.end():]
    else:
        if re.search(r'^\s*import ' + re.escape(prefix) + r'\.\{.*\*\}', s, re.M): return False
        line = 'import ' + prefix + '.{' + ', '.join(tick(n) for n in names) + '}'
        pm = re.search(r'^package [\w.]+\s*\n(package [\w.]+\s*\n)*', s, re.M)
        if pm: s = s[:pm.end()] + '\n' + line + '\n' + s[pm.end():]
        else:
            im = re.search(r'^import ', s, re.M)
            if not im: print('import-suggestions: nowhere to anchor an import in', f); return False
            s = s[:im.start()] + line + '\n' + s[im.start():]
    open(f, 'w').write(s); return True

def hide(f):
    s = open(f).read(); o = s
    s = re.sub(r'^(\s*)import okay\.\{[^}]*\}\s*$', lambda m: m.group(1) + HIDE, s, count=1, flags=re.M)
    if s == o:
        pm = re.search(r'^package [\w.]+\s*\n(package [\w.]+\s*\n)*', s, re.M)
        s = s[:pm.end()] + '\n' + HIDE + '\n' + s[pm.end():]
    open(f, 'w').write(s)

def repair(log):
    txt = open(log, errors='replace').read()
    sugg, fsugg, amb, files = {}, {}, set(), set()
    for b in re.split(r'(?m)^\[error\] -- ', txt):
        m = re.search(r'Error: (/\S+\.scala):\d+:\d+', b)
        if not m: continue
        f = m.group(1); files.add(f)
        if '/okay-cont/' in f or not os.path.exists(f): continue
        has_wild = re.search(r'^\s*import okay\.freer\.\*', open(f).read(), re.M)
        for n in re.findall(r'^\[error\] *\|\s+import okay\.(\S+)\s*$', b, re.M):
            if '.' not in n: sugg.setdefault(f, set()).add(n)
        for n in re.findall(r'Not found: (?:type |value )?([^\s-]+)', b) + re.findall(r'value (\S+) is not a member of', b) + re.findall(r'Expected a (?:type|term), but found a (?:term|type): ([^\s-]+)', b):
            if n in CORE: sugg.setdefault(f, set()).add(n)
            elif n in FREER and not has_wild: fsugg.setdefault(f, set()).add(n)
        if re.search(r'is ambiguous\.\n.*imported by name by import okay\.', b): amb.add(f)
    n = 0
    for f in amb: hide(f); n += 1
    for f, names in sugg.items():
        if f not in amb and add_to(f, 'okay', sorted(names)): n += 1
    for f, names in fsugg.items():
        if f not in amb and add_to(f, 'okay.freer', sorted(names)): n += 1
    return len(files), n

def strip(log):
    lines = open(log, errors='replace').read().split('\n'); items = {}
    for i, l in enumerate(lines):
        m = re.search(r'Unused Symbol Warning: (/\S+\.scala):(\d+):(\d+)', l)
        if m and i + 1 < len(lines):
            q = re.match(r'\[warn\] +\d+ \|(.*)$', lines[i + 1])
            if q: items[(m.group(1), int(m.group(2)))] = (q.group(1).rstrip(), int(m.group(3)))
    byfile = {}
    for (f, ln), t in items.items(): byfile.setdefault(f, []).append((ln, t))
    removed = other = 0
    for f, its in byfile.items():
        if not os.path.exists(f): continue
        src = open(f).read().split('\n')
        for ln, (t, col) in sorted(its, reverse=True):
            if not (ln - 1 < len(src) and src[ln - 1] == t and re.match(r'\s*import ', t)): other += 1; continue
            b = re.match(r'^(\s*import [\w.]+\.\{)([^}]*)(\}.*)$', t)
            if b:
                pos = len(b.group(1)); keep = []
                for sel in b.group(2).split(','):
                    end = pos + len(sel)
                    if not (pos <= col - 1 < end + 1): keep.append(sel)
                    pos = end + 1
                keep = [k.strip() for k in keep if k.strip()]
                if keep: src[ln - 1] = b.group(1) + ', '.join(keep) + b.group(3)
                else: del src[ln - 1]
            else: del src[ln - 1]
            removed += 1
        open(f, 'w').write('\n'.join(src))
    return len(items), removed, other

for it in range(12):
    subprocess.run(['sh', 'scripts/gate.sh', CMD], capture_output=True, text=True)
    log = latest_log(); txt = open(log, errors='replace').read()
    nerr = len(re.findall(r'Error: /', txt))
    ef, rep = repair(log); nw, rm, oth = strip(log)
    print(f'round {it}: errors {nerr} in {ef} files, repaired {rep}; unused {nw}, stripped {rm}, other {oth}; log {log}', flush=True)
    if nerr == 0 and nw == 0: print('CLEAN'); break
    if ef and rep == 0 and rm == 0: print('STUCK'); break
