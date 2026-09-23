# okay-py shim, version 3 (specs/py.md; v2 = foreign-typed-calls: a dict
# is a record on the wire, a frame only where a frame is asked; v3 =
# foreign-callbacks: `start`/`resume` and the injected `okay` module; v4 =
# foreign-object-handles: `hold`/`method`/`attr`/`release`, refs as values;
# v5 = foreign-module-trait: `okay.describe`). Stdlib only, deliberately:
# json wire, one object per line each way; functions are ADDRESSED
# as module:qualified.name and imported, never eval'd from source.
# A failing call answers a condition and the worker survives; only a
# broken wire ends the process.
import sys, json, base64, importlib, importlib.metadata, math, dataclasses, types, inspect

SHIM = 5

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

def reply(obj):
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()

# ---- callbacks into okay (v3) ------------------------------------------
#
# `import okay; okay.call("name", *args)` inside a function okay started
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
        raise RuntimeError("okay.call(%r) outside a call okay started with callbacks" % name)
    if name not in _offered[-1]:
        raise LookupError("okay.call(%r): this call was offered %s" % (name, sorted(_offered[-1])))
    _next_k[0] += 1
    k = _next_k[0]
    reply({"ask": {"cb": name, "args": [enc(a) for a in args], "k": k}})
    while True:
        line = sys.stdin.readline()
        if not line:
            raise SystemExit(0)          # the host is gone
        if not line.strip():
            continue
        req = json.loads(line)
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

okay_module = types.ModuleType("okay")
okay_module.describe = _describe
okay_module.call = _call
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
        elif op == "resume":
            raise ValueError("resume %r: no call is waiting for it (resumed twice?)" % req.get("k"))
        else:
            raise ValueError("unknown op %r" % op)
    except SystemExit:
        raise
    except Exception as e:
        reply({"id": rid, "condition": {"kind": type(e).__name__, "message": str(e)}})

reply({"shim": SHIM, "python": "%d.%d.%d" % sys.version_info[:3]})

# readline, not `for line in sys.stdin`: okay.call reads the same stream
# from inside a request, and one reader must own the buffer
while True:
    line = sys.stdin.readline()
    if not line:
        break
    if not line.strip():
        continue
    serve(json.loads(line))
