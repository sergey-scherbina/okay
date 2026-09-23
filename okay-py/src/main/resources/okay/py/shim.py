# okay-py shim, version 2 (specs/py.md; v2 = foreign-typed-calls:
# a dict is a record on the wire, a frame only where a frame is asked). Stdlib only, deliberately:
# json wire, one object per line each way; functions are ADDRESSED
# as module:qualified.name and imported, never eval'd from source.
# A failing call answers a condition and the worker survives; only a
# broken wire ends the process.
import sys, json, base64, importlib, importlib.metadata, math, dataclasses

SHIM = 2

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

reply({"shim": SHIM, "python": "%d.%d.%d" % sys.version_info[:3]})

for line in sys.stdin:
    if not line.strip():
        continue
    req = json.loads(line)
    rid = req.get("id")
    try:
        op = req["op"]
        if op == "call":
            f = resolve(req["fn"])
            out = f(*[dec(a) for a in req.get("args", [])])
            reply({"id": rid, "ok": enc(out)})
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
        else:
            raise ValueError("unknown op %r" % op)
    except Exception as e:
        reply({"id": rid, "condition": {"kind": type(e).__name__, "message": str(e)}})
