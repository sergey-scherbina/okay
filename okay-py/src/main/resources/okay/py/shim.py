# okay-py shim, version 3 (specs/py.md; v2 = foreign-typed-calls: a dict
# is a record on the wire, a frame only where a frame is asked; v3 =
# foreign-callbacks: `start`/`resume` and the injected `okay` module; v4 =
# foreign-object-handles: `hold`/`method`/`attr`/`release`, refs as values;
# v5 = foreign-module-trait: `okay.describe`; v6 = remote-foreign: programs
# as data, `program`/`continue`/`forget`, continuations kept by id). Stdlib only, deliberately:
# json wire, one object per line each way; functions are ADDRESSED
# as module:qualified.name and imported, never eval'd from source.
# A failing call answers a condition and the worker survives; only a
# broken wire ends the process.
import sys, json, base64, importlib, importlib.metadata, math, dataclasses, types, inspect, struct, zlib

SHIM = 6

# a JSON number is a double: exact only up to 2**53
EXACT = 2 ** 53

def enc(v):
    if v is None: return None
    if isinstance(v, bool): return v
    if isinstance(v, float):
        if math.isnan(v): return {"t": "nan"}
        # an integral float would MERGE with int on the json wire;
        # the tag keeps 3.0 being a float on the way back
        if v == int(v) and abs(v) < 1e15: return {"t": "f", "v": v}
        return v
    if isinstance(v, int):
        if abs(v) >= EXACT: return {"t": "int", "v": str(v)}
        return v
    if isinstance(v, str): return v
    if isinstance(v, (bytes, bytearray)):
        return {"t": "bytes", "b64": base64.b64encode(bytes(v)).decode()}
    # a dataclass is a record: its fields, in declaration order
    if dataclasses.is_dataclass(v) and not isinstance(v, type):
        v = {f.name: getattr(v, f.name) for f in dataclasses.fields(v)}
    if isinstance(v, dict):
        if not all(isinstance(k, str) for k in v):
            raise TypeError("a dict crosses only with string keys")
        return {"t": "dict", "kv": [[k, enc(x)] for k, x in v.items()]}
    if isinstance(v, (list, tuple)):
        return [enc(x) for x in v]
    # an unknown type is a CONDITION at the call site, not a guess here
    raise TypeError("cannot encode a %s for the wire" % type(v).__name__)

# ---- held objects (v4) --------------------------------------------------

_held = {}
_next_ref = [0]

def hold(obj):
    _next_ref[0] += 1
    i = _next_ref[0]
    _held[i] = obj
    t = type(obj)
    return {"t": "ref", "id": i, "type": "%s.%s" % (t.__module__, t.__qualname__)}

def held(i):
    if i not in _held:
        raise LookupError("ref %s is not held by this process "
                          "(released, or held by a process that is gone)" % i)
    return _held[i]

def enc_frame(v):
    # a frame function's answer: a dict of columns, or anything with a
    # pandas-style to_dict(orient="list")
    if hasattr(v, "to_dict") and not isinstance(v, dict):
        v = v.to_dict(orient="list")
    if not isinstance(v, dict):
        raise TypeError("a frame function must answer a dict of columns, got %s"
                        % type(v).__name__)
    return {"t": "frame", "cols": [[k, [enc(x) for x in col]] for k, col in v.items()]}

def dec(v):
    if isinstance(v, dict):
        t = v.get("t")
        if t == "dict": return {k: dec(x) for k, x in v["kv"]}
        if t == "int": return int(v["v"])
        if t == "ref": return held(v["id"])
        if t == "nan": return float("nan")
        if t == "f": return float(v["v"])
        if t == "bytes": return base64.b64decode(v["b64"])
        if t == "frame": return {name: [dec(x) for x in col] for name, col in v["cols"]}
        raise ValueError("unknown tagged value: %r" % t)
    if isinstance(v, list): return [dec(x) for x in v]
    return v

def resolve(fn):
    mod, _, name = fn.partition(":")
    if not name:
        raise ValueError("a function is addressed as module:name, got %r" % fn)
    obj = importlib.import_module(mod)
    for part in name.split("."):
        obj = getattr(obj, part)
    return obj

# ---- the wire's encoding (polyglot-one-wire stage 5a) --------------------
#
# JSON lines until the host configures otherwise; then FRAMES (a 4-byte
# big-endian length, then the message), each message the same tree encoded
# as JSON or CBOR and optionally raw-DEFLATEd. CBOR here is the wire's
# subset, in the standard library: no package is required for it.

_IN = sys.stdin.buffer
_OUT = sys.stdout.buffer
_mode = {"format": "json", "compress": "none"}

def _framed():
    return _mode["format"] != "json" or _mode["compress"] != "none"

def _cbor_head(out, major, n):
    m = major << 5
    if n < 24: out.append(m | n)
    elif n < 1 << 8: out += bytes([m | 24, n])
    elif n < 1 << 16: out += bytes([m | 25]) + n.to_bytes(2, "big")
    elif n < 1 << 32: out += bytes([m | 26]) + n.to_bytes(4, "big")
    else: out += bytes([m | 27]) + n.to_bytes(8, "big")

def _cbor_enc(out, v):
    if v is None: out.append(0xf6)
    elif v is True: out.append(0xf5)
    elif v is False: out.append(0xf4)
    elif isinstance(v, int):
        if v >= 0: _cbor_head(out, 0, v)
        else: _cbor_head(out, 1, -1 - v)
    elif isinstance(v, float): out += b"\xfb" + struct.pack(">d", v)
    elif isinstance(v, str):
        b = v.encode("utf-8"); _cbor_head(out, 3, len(b)); out += b
    elif isinstance(v, (list, tuple)):
        _cbor_head(out, 4, len(v))
        for x in v: _cbor_enc(out, x)
    elif isinstance(v, dict):
        _cbor_head(out, 5, len(v))
        for k, x in v.items(): _cbor_enc(out, k); _cbor_enc(out, x)
    else: raise TypeError("a %s does not encode as CBOR" % type(v).__name__)

def _cbor_dec(b, i):
    if i >= len(b): raise ValueError("a CBOR message ended early (cut short?)")
    ib = b[i]; i += 1
    major, info = ib >> 5, ib & 0x1f
    def arg(i):
        if info < 24: return info, i
        n = {24: 1, 25: 2, 26: 4, 27: 8}.get(info)
        if n is None: raise ValueError("CBOR: an indefinite or reserved length (%d) is not in the wire's subset" % info)
        if i + n > len(b): raise ValueError("a CBOR message ended early (cut short?)")
        return int.from_bytes(b[i:i + n], "big"), i + n
    if major == 7:
        if info == 20: return False, i
        if info == 21: return True, i
        if info in (22, 23): return None, i
        if info in (25, 26, 27):
            n = {25: 2, 26: 4, 27: 8}[info]
            if i + n > len(b): raise ValueError("a CBOR message ended early (cut short?)")
            return struct.unpack({25: ">e", 26: ">f", 27: ">d"}[info], b[i:i + n])[0], i + n
        raise ValueError("CBOR: simple value %d is not in the wire's subset" % info)
    n, i = arg(i)
    if major == 0: return n, i
    if major == 1: return -1 - n, i
    if major == 3:
        if i + n > len(b): raise ValueError("a CBOR string ended early (cut short?)")
        return b[i:i + n].decode("utf-8"), i + n
    if major == 4:
        xs = []
        for _ in range(n):
            x, i = _cbor_dec(b, i); xs.append(x)
        return xs, i
    if major == 5:
        m = {}
        for _ in range(n):
            k, i = _cbor_dec(b, i); x, i = _cbor_dec(b, i)
            if not isinstance(k, str): raise ValueError("CBOR: a map key that is not text")
            m[k] = x
        return m, i
    raise ValueError("CBOR: major type %d (byte strings, tags) is not in the wire's subset" % major)

def _encode(obj):
    if _mode["format"] == "cbor":
        out = bytearray(); _cbor_enc(out, obj); data = bytes(out)
    else:
        data = json.dumps(obj).encode("utf-8")
    if _mode["compress"] == "deflate":
        z = zlib.compressobj(zlib.Z_DEFAULT_COMPRESSION, zlib.DEFLATED, -15)
        data = z.compress(data) + z.flush()
    return data

def _decode(data):
    if _mode["compress"] == "deflate":
        data = zlib.decompress(data, -15)
    if _mode["format"] == "cbor":
        v, i = _cbor_dec(data, 0)
        if i != len(data): raise ValueError("CBOR: %d bytes after the message" % (len(data) - i))
        return v
    return json.loads(data)

def reply(obj):
    if _framed():
        data = _encode(obj)
        _OUT.write(len(data).to_bytes(4, "big") + data)
    else:
        _OUT.write(json.dumps(obj).encode("utf-8") + b"\n")
    _OUT.flush()

def read_msg():
    """the next request, or None when the host is gone"""
    while True:
        if _framed():
            n = _IN.read(4)
            if len(n) < 4: return None
            data = _IN.read(int.from_bytes(n, "big"))
            return _decode(data)
        line = _IN.readline()
        if not line: return None
        if line.strip(): return json.loads(line)

# ---- callbacks into okay (v3) ------------------------------------------
#
# `from okay import okay_call; okay_call("name", *args)` (the one name in
# every language okay speaks; `okay.call` is its old alias) inside a function okay started
# with callbacks: the ask goes to the host, and THIS frame waits for the
# resume. While it waits, any request that arrives is served (a callback
# may call Python again on this very worker: the nesting is strict, so
# one wire carries it). The waiting frame is resumed ONCE.

class OkayError(Exception):
    """a callback that failed in okay: its condition's kind and message"""
    def __init__(self, kind, message):
        super().__init__("%s: %s" % (kind, message))
        self.kind = kind
        self.message = message

_offered = []      # a stack: the callback names each active start offered
_next_k = [0]

def _call(name, *args):
    if not _offered:
        raise RuntimeError("okay_call(%r) outside a call okay started with callbacks" % name)
    if name not in _offered[-1]:
        raise LookupError("okay_call(%r): this call was offered %s" % (name, sorted(_offered[-1])))
    _next_k[0] += 1
    k = _next_k[0]
    reply({"ask": {"cb": name, "args": [enc(a) for a in args], "k": k}})
    while True:
        req = read_msg()
        if req is None:
            raise SystemExit(0)          # the host is gone
        if req.get("op") == "resume" and req.get("k") == k:
            if "condition" in req:
                c = req["condition"]
                raise OkayError(c.get("kind", ""), c.get("message", ""))
            return dec(req.get("ok"))
        serve(req)                       # a nested request, answered in turn

def _ann(a):
    if a is inspect.Parameter.empty:
        return ""
    if isinstance(a, str):
        return a
    if isinstance(a, type):
        return a.__name__
    return str(a).replace("typing.", "")

def _describe(module):
    # what okay's PyFacade writes a Scala object from (foreign-module-trait):
    # the module's own public functions, in name order, as they are declared
    mod = importlib.import_module(module)
    out = []
    for name, fn in inspect.getmembers(mod, inspect.isfunction):
        if name.startswith("_") or fn.__module__ != mod.__name__:
            continue
        sig = inspect.signature(fn)
        params = [{"name": p.name, "ann": _ann(p.annotation), "default": p.default is not p.empty}
                  for p in sig.parameters.values()
                  if p.kind not in (p.VAR_POSITIONAL, p.VAR_KEYWORD)]
        doc = (inspect.getdoc(fn) or "").split("\n")[0]
        out.append({"name": name, "params": params, "returns": _ann(sig.return_annotation), "doc": doc})
    return out

# ---- programs as data (v6, remote-foreign) ------------------------------
#
# okay.done(v) and okay.perform(name, *args).then(f): a program is a TREE
# whose continuations are plain functions. The shim hands okay one node at
# a time and keeps each continuation in a table under an id, so okay may
# continue the SAME id more than once (a Choice handler does) - as long as
# the function is pure, every branch is exact. A run's continuations live
# until okay forgets the run.

class Done:
    def __init__(self, value):
        self.value = value
    def then(self, f):
        return f(self.value)

class Step:
    def __init__(self, name, args, k):
        self.name, self.args, self.k = name, args, k
    def then(self, f):
        k = self.k
        return Step(self.name, self.args, lambda x: k(x).then(f))

def _done(value):
    return Done(value)

def _perform(name, *args):
    return Step(name, list(args), _done)

_runs = {}
_next_kont = [0]

def _node(run, p):
    if isinstance(p, Done):
        return {"done": enc(p.value)}
    if isinstance(p, Step):
        _next_kont[0] += 1
        k = _next_kont[0]
        _runs.setdefault(run, {})[k] = p.k
        return {"perform": p.name, "args": [enc(a) for a in p.args], "k": k}
    raise TypeError("a program answers okay.done(v) or okay.perform(name, ...), got %s"
                    % type(p).__name__)

okay_module = types.ModuleType("okay")
okay_module.done = _done
okay_module.perform = _perform
okay_module.Done = Done
okay_module.Step = Step
okay_module.describe = _describe
okay_module.okay_call = _call
okay_module.call = _call          # the old name, kept
okay_module.OkayError = OkayError
sys.modules["okay"] = okay_module

def serve(req):
    rid = req.get("id")
    try:
        op = req["op"]
        if op == "call":
            f = resolve(req["fn"])
            out = f(*[dec(a) for a in req.get("args", [])])
            reply({"id": rid, "ok": enc(out)})
        elif op == "start":
            f = resolve(req["fn"])
            _offered.append(set(req.get("callbacks", [])))
            try:
                out = f(*[dec(a) for a in req.get("args", [])])
            finally:
                _offered.pop()
            reply({"id": rid, "ok": enc(out)})
        elif op == "program":
            f = resolve(req["fn"])
            reply({"id": rid, "ok": _node(req["run"], f(*[dec(a) for a in req.get("args", [])]))})
        elif op == "continue":
            run, k = req["run"], req["k"]
            if run not in _runs or k not in _runs[run]:
                raise LookupError("continuation %s of run %s is not held here (forgotten, or another process)"
                                  % (k, run))
            reply({"id": rid, "ok": _node(run, _runs[run][k](dec(req.get("answer"))))})
        elif op == "forget":
            _runs.pop(req["run"], None)
            reply({"id": rid, "ok": None})
        elif op == "hold":
            f = resolve(req["fn"])
            reply({"id": rid, "ok": hold(f(*[dec(a) for a in req.get("args", [])]))})
        elif op == "method":
            m = getattr(held(req["ref"]), req["name"])
            out = m(*[dec(a) for a in req.get("args", [])])
            reply({"id": rid, "ok": hold(out) if req.get("hold") else enc(out)})
        elif op == "attr":
            reply({"id": rid, "ok": enc(getattr(held(req["ref"]), req["name"]))})
        elif op == "release":
            _held.pop(req["ref"], None)
            reply({"id": rid, "ok": None})
        elif op == "frame":
            f = resolve(req["fn"])
            frame = dec(req["in"])
            out = f(frame, *[dec(a) for a in req.get("args", [])])
            reply({"id": rid, "ok": enc_frame(out)})
        elif op == "verify":
            pkgs = {}
            for name in req.get("packages", []):
                try:
                    pkgs[name] = importlib.metadata.version(name)
                except importlib.metadata.PackageNotFoundError:
                    pkgs[name] = None
            reply({"id": rid, "ok": {"python": "%d.%d.%d" % sys.version_info[:3],
                                     "packages": pkgs}})
        elif op == "configure":
            f, c = req.get("format"), req.get("compress")
            if f not in ("json", "cbor"):
                raise ValueError("this Python worker speaks the formats json, cbor; not %r" % f)
            if c not in ("none", "deflate"):
                raise ValueError("this Python worker speaks the compressions none, deflate; not %r" % c)
            reply({"id": rid, "ok": {"format": f, "compress": c}})
            _mode["format"], _mode["compress"] = f, c    # AFTER its own answer
        elif op == "resume":
            raise ValueError("resume %r: no call is waiting for it (resumed twice?)" % req.get("k"))
        else:
            raise ValueError("unknown op %r" % op)
    except SystemExit:
        raise
    except Exception as e:
        reply({"id": rid, "condition": {"kind": type(e).__name__, "message": str(e)}})

reply({"shim": SHIM, "python": "%d.%d.%d" % sys.version_info[:3],
       "speaks": {"format": ["json", "cbor"], "compress": ["deflate"]}})

# one reader, read_msg, owns the input: okay.call reads the same stream from
# inside a request
while True:
    req = read_msg()
    if req is None:
        break
    serve(req)
