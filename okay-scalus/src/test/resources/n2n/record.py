"""Independent N2N probe: records a preprod relay session (both directions, raw mux segments)."""
import socket, struct, time, sys, json, hashlib, cbor2, urllib.request

HOST, PORT, MAGIC = "preprod-node.play.dev.cardano.org", 3001, 1
OUT = sys.argv[1]
blocks = json.load(urllib.request.urlopen(
    "https://preprod.koios.rest/api/v1/blocks?limit=6&select=hash,abs_slot,block_height"))
start = blocks[5]   # intersect 5 blocks back
t0 = time.time()
log = []            # (dir, header8hex, payloadhex)

s = socket.create_connection((HOST, PORT), timeout=30)
def send(proto, obj):
    payload = cbor2.dumps(obj)
    ts = int((time.time() - t0) * 1e6) & 0xffffffff
    hdr = struct.pack(">IHH", ts, proto & 0x7fff, len(payload))
    s.sendall(hdr + payload); log.append(("out", hdr.hex(), payload.hex()))
buf = {}
def recv_exact(n):
    b = b""
    while len(b) < n:
        c = s.recv(n - len(b))
        if not c: raise EOFError
        b += c
    return b
def recv_msg(proto):
    """read segments until one whole CBOR item for `proto` is buffered"""
    while True:
        data = buf.get(proto, b"")
        if data:
            try:
                import io
                f = io.BytesIO(data); obj = cbor2.CBORDecoder(f).decode(); used = f.tell()
                buf[proto] = data[used:]; return obj, data[:used]
            except Exception:
                pass
        hdr = recv_exact(8); ts, pm, ln = struct.unpack(">IHH", hdr)
        payload = recv_exact(ln); log.append(("in", hdr.hex(), payload.hex()))
        p = pm & 0x7fff
        buf[p] = buf.get(p, b"") + payload

vd14 = [MAGIC, True, 0, False]
send(0, [0, {14: vd14, 15: vd14, 16: vd14 + [False]}])
hs, _ = recv_msg(0); print("handshake:", hs)
send(2, [4, [[start["abs_slot"], bytes.fromhex(start["hash"])]]])
m, _ = recv_msg(2); print("intersect:", m[0], m[1] if len(m) > 1 else "")
headers = []
for _ in range(12):
    send(2, [0]); m, raw = recv_msg(2)
    if m[0] == 1:
        print("await"); break
    if m[0] == 3: print("rollback to", m[1]); continue
    if m[0] == 2:
        hdr = m[1]   # [era, tag24(bytes)]
        era = hdr[0]; hb = hdr[1].value
        h = hashlib.blake2b(hb, digest_size=32).hexdigest()
        body = cbor2.loads(hb)[0]
        print("forward era", era, "block", body[0], "slot", body[1], "hash", h[:16], "prev", (body[2] or b"").hex()[:16])
        headers.append((body[1], h))
if headers:
    a, b = headers[0], headers[-1]
    send(3, [0, [a[0], bytes.fromhex(a[1])], [b[0], bytes.fromhex(b[1])]])
    while True:
        m, raw = recv_msg(3)
        if m[0] == 4:
            inner = m[1].value if hasattr(m[1], "value") else m[1]
            era, blk = cbor2.loads(inner)
            print("block era", era, "txs", len(blk[1]))
        else:
            print("block-fetch msg", m[0])
        if m[0] in (3, 5): break
    send(3, [1])
send(2, [7])
s.close()
json.dump({"host": HOST, "magic": MAGIC, "intersect": start, "koios": blocks, "segments": log,
           "recorded": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}, open(OUT, "w"), indent=1)
print("koios hashes:", [b["hash"][:16] for b in blocks[:5]][::-1])
print("segments:", len(log))
