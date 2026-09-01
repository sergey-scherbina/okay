# okay-py shim, version 1 (specs/py.md). Stdlib only, deliberately:
# json wire, one object per line each way; functions are ADDRESSED
# as module:qualified.name and imported, never eval'd from source.
# A failing call answers a condition and the worker survives; only a
# broken wire ends the process.
import sys, json, base64, importlib, importlib.metadata, math

SHIM = 1

def enc(v):
    if v is None: return None
    if isinstance(v, bool): return v
    if isinstance(v, float):
        if math.isnan(v): return {"t": "nan"}
        # an integral float would MERGE with int on the json wire;
        # the tag keeps 3.0 being a float on the way back
        if v == int(v) and abs(v) < 1e15: return {"t": "f", "v": v}
        return v
    if isinstance(v, int): return v
    if isinstance(v, str): return v
    if isinstance(v, (bytes, bytearray)):
        return {"t": "bytes", "b64": base64.b64encode(bytes(v)).decode()}
    if isinstance(v, dict) and all(isinstance(k, str) for k in v):
        return {"t": "frame", "cols": [[k, [enc(x) for x in col]] for k, col in v.items()]}
    if isinstance(v, (list, tuple)):
        return [enc(x) for x in v]
    # an unknown type is a CONDITION at the call site, not a guess here
    raise TypeError("cannot encode a %s for the wire" % type(v).__name__)

def dec(v):
    if isinstance(v, dict):
        t = v.get("t")
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
            if not isinstance(out, dict):
                raise TypeError("a frame function must answer a dict of columns, got %s"
                                % type(out).__name__)
            reply({"id": rid, "ok": enc(out)})
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
