#!/usr/bin/env python3
"""recscan: STACK recursion left after tail-call elimination.
Call graph per class over its methods; a lambda is an edge from the
method that CREATES it (invokedynamic) to its body, labelled by the call
that consumes it (the next invoke). An edge whose consumer defers the
body (a program's flatMap/map, Cont, a thunk, a lazy cell) is dropped:
a cycle through it is trampolined. Every cycle left is stack recursion.
Output: file:line  def  kind  consumer-of-lambda(s) on the cycle
usage: recscan.py <root> [skip-prefix ...]"""
import os, re, subprocess, sys, collections
root = sys.argv[1]; skips = sys.argv[2:]
DEFER_OWNER = re.compile(r'^(okay2?/Free|okay2?/Free\$.*|okay2?/Cont.*|okay2?/.*package\$\!|okay/package\$|okay2/package\$|okay2?/Thunk.*|okay/frege/Thunk.*|scala/collection/immutable/LazyList.*|scala/Function0|okay2?/Once.*|okay2?/Eval.*|okay2?/\$bang\$.*|okay2?/Row\$.*|okay/async/.*|okay2/async/.*)$')
DEFER_NAME = re.compile(r'^(flatMap|map|andThen|defer|delay|suspend|onAnswer|foldCont|lazy|shared|bind|then|flatMap\w*|map\w*|resumeWith|tailcall|\$greater\$greater\$eq|\$times\$greater|as|void|attempt|handleWith)$')

def class_dirs(root):
    for d, subs, _ in os.walk(root):
        subs[:] = [s for s in subs if s not in ('node_modules', '.git', '.bsp', 'streams', 'zinc', 'test-classes', 'src_managed', 'resolution-cache')]
        if re.search(r'/target/scala-[^/]+/classes$', d) and not re.search(r'/(\.js|\.native|js|native)/target', d) and '/project/' not in d:
            if any(os.path.relpath(d, root).startswith(s) for s in skips): continue
            yield d

def scala_index(root):
    idx = collections.defaultdict(list)
    for d, subs, files in os.walk(root):
        subs[:] = [s for s in subs if s not in ('target', 'node_modules', '.git', '.bsp', 'test', 'jmh')]
        for f in files:
            if f.endswith('.scala'): idx[f].append(os.path.join(d, f))
    return idx

def parse(files):
    out = subprocess.run(['javap', '-c', '-p', '-s', '-l', '-v'] + files, capture_output=True, text=True).stdout
    res = {}; c = None; cur = None; inboot = False; bi = None
    for line in out.splitlines():
        if line.startswith('Classfile '):
            c = {'name': None, 'methods': [], 'boot': {}, 'src': '?'}; res[line.split(' ', 1)[1]] = c; cur = None; inboot = False; continue
        tm = re.match(r'^(?:public |final |abstract |private |protected |static |synchronized |sealed )*(?:class|interface) ([\w.$]+)', line)
        if tm and c['name'] is None: c['name'] = tm.group(1).replace('.', '/'); continue
        if line.startswith('BootstrapMethods:'): inboot = True; cur = None; continue
        if inboot:
            bm = re.match(r'^\s+(\d+): #\d+', line)
            if bm: bi = int(bm.group(1)); continue
            im = re.search(r'REF_invoke\w+ ([\w/$]+)\.([\w$]+):(\(\S+)', line)
            if im and bi is not None and im.group(1) == c['name']: c['boot'][bi] = (im.group(2), im.group(3))
            sm = re.match(r'^SourceFile: "(.+)"$', line)
            if sm: c['src'] = sm.group(1)
            continue
        sm = re.match(r'^SourceFile: "(.+)"$', line)
        if sm: c['src'] = sm.group(1); continue
        m = re.match(r'^  (?:[\w$<>\[\], .?]*\s)?([\w$<>]+)\(.*\);$', line)
        if m: cur = {'name': m.group(1), 'desc': None, 'ins': [], 'lines': []}; c['methods'].append(cur); continue
        if cur is None: continue
        dm = re.match(r'^    descriptor: (\S+)', line)
        if dm and cur['desc'] is None: cur['desc'] = dm.group(1); continue
        yd = re.search(r'invokedynamic\s+#\d+,\s*\d+\s+// InvokeDynamic #(\d+):', line)
        if yd: cur['ins'].append(('dyn', int(yd.group(1)))); continue
        im = re.search(r'invoke(?:virtual|static|special|interface)\s+#\d+(?:,\s*\d+)?\s+// (?:Interface)?Method (?:([\w/$]+)\.)?("?[\w$<>]+"?):(\S+)', line)
        if im: cur['ins'].append(('call', (im.group(1) or c['name'], im.group(2).strip('"'), im.group(3)))); continue
        lm = re.match(r'^\s+line (\d+): \d+$', line)
        if lm: cur['lines'].append(int(lm.group(1)))
    return res

def sccs(nodes, edges):
    sys.setrecursionlimit(1000000)
    idx = {}; low = {}; st = []; on = set(); res = []; k = [0]
    def sc(v):
        idx[v] = low[v] = k[0]; k[0] += 1; st.append(v); on.add(v)
        for w in edges.get(v, ()):
            if w not in idx: sc(w); low[v] = min(low[v], low[w])
            elif w in on: low[v] = min(low[v], idx[w])
        if low[v] == idx[v]:
            comp = []
            while True:
                w = st.pop(); on.discard(w); comp.append(w)
                if w == v: break
            if len(comp) > 1 or v in edges.get(v, ()): res.append(comp)
    for v in nodes:
        if v not in idx: sc(v)
    return res

def is_lambda(n): return 'anonfun' in n

idx = scala_index(root); rows = set()
for d in class_dirs(root):
    files = [os.path.join(dp, f) for dp, _, fs in os.walk(d) for f in fs if f.endswith('.class')]
    for i in range(0, len(files), 250):
        for cf, c in parse(files[i:i+250]).items():
            if not c['name']: continue
            nodes = {(m['name'], m['desc']): m for m in c['methods']}
            edges = collections.defaultdict(set); via = {}
            for key, m in nodes.items():
                ins = m['ins']
                for j, (kind, x) in enumerate(ins):
                    if kind == 'call':
                        o, n, ds = x
                        if o == c['name'] and (n, ds) in nodes: edges[key].add((n, ds))
                    else:
                        tgt = c['boot'].get(x)
                        if not tgt or tgt not in nodes: continue
                        # the consumer: the next call that is not boxing/runtime plumbing
                        cons = next((y for kk, y in ins[j+1:] if kk == 'call' and not y[0].startswith(('scala/runtime/', 'java/lang/Integer', 'java/lang/Long', 'java/lang/Boolean', 'java/lang/Double'))), None)
                        deferred = cons is not None and (
                            DEFER_OWNER.match(cons[0]) and DEFER_NAME.match(cons[1])
                            or cons[1] in ('delay', 'defer', 'suspend', 'lazy', 'shared', 'tailcall')
                            # an inlined flatMap builds the node itself
                            or re.search(r'Free\$(Bind|Delay|Suspend)\$$', cons[0]) and cons[1] == 'apply'
                            # Scala 2 builds the node with `new`
                            or re.search(r'Free\$(Bind|Delay|Suspend)$', cons[0]) and cons[1] == '<init>'
                            or re.search(r'Safepoint\$$', cons[0]) and 'defer' in cons[1]
                            # a callback registered now, run later from another frame
                            or cons[1] == '<init>' and re.search(r'Waiter$', cons[0])
                            # `a #:: rest`: the tail is a by-name cell
                            or cons[1] == 'toDeferrer' and 'LazyList' in cons[0])
                        via[(key, tgt)] = (cons[0].split('/')[-1] + '.' + cons[1]) if cons else '?'
                        if not deferred: edges[key].add(tgt)
            for comp in sccs(list(nodes), edges):
                cs = set(comp)
                real = [k for k in comp if not is_lambda(k[0])]
                if not real: continue
                kind = 'direct' if any(k in edges[k] for k in real) else ('lambda' if len(real) == 1 else 'mutual')
                cons = sorted({v for (a, b), v in via.items() if a in cs and b in cs})
                for k in real:
                    m = nodes[k]
                    if m['lines']: rows.add((c['src'], min(m['lines']), k[0], kind, ','.join(cons)[:80], os.path.relpath(cf, d), re.sub(r'/(\.jvm|jvm)$', '', d.split('/target/')[0])))

out = set()
for src, line, name, kind, cons, cls, mod in rows:
    base = re.sub(r'\$\d+$', '', re.sub(r'^.*\$\$', '', name))
    cands = [x for x in idx.get(src, []) if x.startswith(mod + '/src/')]
    pkg = os.path.dirname(cls).split('/')[-1:]
    near = [x for x in cands if x.split('/')[-2:-1] == pkg] or cands
    for p in near:
        t = open(p).read().split('\n')
        if line > len(t): continue
        dl = next((j for j in range(line - 1, max(-1, line - 400), -1) if re.search(r'\bdef\s+' + re.escape(base) + r'\b', t[j])), None)
        if dl is None: continue
        out.add((os.path.relpath(p, root), dl + 1, base, kind, cons)); break
for r in sorted(out): print('\t'.join(map(str, r)))
