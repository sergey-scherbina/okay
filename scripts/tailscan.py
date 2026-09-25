#!/usr/bin/env python3
"""tailscan: methods scalac already compiled to a loop (a `goto 0`, the
jump its tail-call elimination emits, Scala 2 and 3 alike), mapped back
to their `def` by line number; reports whether the def carries @tailrec.
usage: tailscan.py <root> [<root>...]  (JVM main classes only)"""
import os, re, subprocess, sys, collections

def class_dirs(root):
    for d, subs, _ in os.walk(root):
        # never descend into another worktree's or node's trees
        subs[:] = [s for s in subs if s not in ('node_modules', '.git', '.bsp', 'streams', 'zinc', 'test-classes', 'src_managed', 'resolution-cache')]
        if d.endswith('/classes') and re.search(r'/target/scala-[^/]+/classes$', d):
            if '/.js/' in d or '/.native/' in d or '/js/target' in d or '/native/target' in d: continue
            if '/project/' in d: continue
            yield d

def scala_index(root):
    idx = collections.defaultdict(list)
    for d, subs, files in os.walk(root):
        subs[:] = [s for s in subs if s not in ('target', 'node_modules', '.git', '.bsp', 'test', 'jmh')]
        for f in files:
            if f.endswith('.scala'): idx[f].append(os.path.join(d, f))
    return idx

def javap(files):
    out = subprocess.run(['javap', '-c', '-l', '-p', '-v'] + files, capture_output=True, text=True).stdout
    cls = src = None; meth = None; methods = []
    for line in out.splitlines():
        if line.startswith('Classfile '):
            cls = line.split(' ', 1)[1]; meth = None
        m = re.match(r'^  (?:[a-z][\w<>\[\], .$]*\s)?([\w$<>]+)\(.*\);$', line)
        if m and not line.startswith('   '):
            meth = {'cls': cls, 'name': m.group(1), 'goto0': False, 'lines': []}
            methods.append(meth); continue
        if meth is not None:
            if re.search(r'^\s+\d+: goto\s+0$', line): meth['goto0'] = True
            lm = re.match(r'^\s+line (\d+): \d+$', line)
            if lm: meth['lines'].append(int(lm.group(1)))
        sm = re.match(r'^SourceFile: "(.+)"$', line)
        if sm:
            for x in methods:
                if x['cls'] == cls and 'src' not in x: x['src'] = sm.group(1)
    return methods

def base(name):
    n = re.sub(r'\$\d+$', '', name)          # loop$1 -> loop
    n = re.sub(r'^.*\$\$', '', n)             # outer$$inner
    return n

def main(roots):
    rows = []
    for root in roots:
        idx = scala_index(root)
        for d in class_dirs(root):
            files = [os.path.join(dp, f) for dp, _, fs in os.walk(d) for f in fs if f.endswith('.class')]
            for i in range(0, len(files), 300):
                for m in javap(files[i:i+300]):
                    if not m['goto0'] or not m['lines'] or 'anonfun' in m['name']: continue
                    name = base(m['name'])
                    pkgdir = os.path.dirname(os.path.relpath(m['cls'], d))
                    cands = idx.get(m.get('src', ''), [])
                    near = [c for c in cands if pkgdir.split('/')[-1:] == c.split('/')[-2:-1]] or cands
                    first = min(m['lines'])
                    for path in near:
                        try: text = open(path).read().splitlines()
                        except OSError: continue
                        if first > len(text): continue
                        defre = re.compile(r'\bdef\s+' + re.escape(name) + r'\b')
                        dl = next((j for j in range(first - 1, max(-1, first - 400), -1) if defre.search(text[j])), None)
                        if dl is None: continue
                        ann = any('tailrec' in text[j] for j in range(max(0, dl - 3), dl + 1))
                        body = '\n'.join(text[dl + 1: dl + 120])
                        selfcall = bool(re.search(r'(?<![\w.])' + re.escape(name) + r'\s*[(\[]', body)) or bool(re.search(r'(?<![\w.])' + re.escape(name) + r'\s*[(\[]', text[dl].split('=', 1)[-1]))
                        rows.append((os.path.relpath(path, root), dl + 1, name, ann, selfcall, root))
                        break
    seen = set()
    for r in sorted(set(rows)):
        key = (r[5], r[0], r[1])
        if key in seen: continue
        seen.add(key)
        print('\t'.join([os.path.basename(r[5].rstrip('/')), r[0] + ':' + str(r[1]), r[2], 'TAILREC' if r[3] else 'MISSING', 'self' if r[4] else 'noself']))

main(sys.argv[1:])
