#!/usr/bin/env python3
"""okay's wire gateway (polyglot-one-wire stage 7): ANY stdio worker on TCP.

    gateway.py --listen HOST:PORT -- WORKER COMMAND...

On every connection it starts WORKER COMMAND, so each connection has a worker
of its own (as Go's ServeTCP and Rust's serve_tcp give each connection a
Worker), and relays between the socket and the worker's stdin/stdout.

The network's layers live HERE, so the workers do not change:
  - TLS, with OKAY_TLS_CERT and OKAY_TLS_KEY (PEM files);
  - the mutual HMAC-SHA256 challenge, with OKAY_WIRE_SECRET or
    OKAY_WIRE_SECRET_FILE: the gateway adds it to the worker's hello, answers
    the host's `auth` itself, refuses every other request until it passes,
    and closes on a wrong mac, as Go and Rust do.
After that, bytes are relayed untouched: a stage-5a `configure` and the frames
after it reach the worker exactly as they would over a pipe.

Once bound it prints {"listening": "host:port", "tls": bool} on stdout.
Standard library only.
"""

import hashlib
import hmac
import json
import os
import secrets
import socket
import ssl
import subprocess
import sys
import threading


def fail(msg):
    sys.stderr.write("okay gateway: %s\n" % msg)
    sys.exit(1)


def secret_from_env():
    s = os.environ.get("OKAY_WIRE_SECRET", "")
    if s:
        return s.encode("utf-8")
    f = os.environ.get("OKAY_WIRE_SECRET_FILE", "")
    if f:
        try:
            with open(f, "rb") as h:
                b = h.read().rstrip(b"\r\n")
        except OSError as e:
            fail("the wire secret's file %s: %s" % (f, e))
        if not b:
            fail("the wire secret's file %s is empty" % f)
        return b
    return None


def tls_from_env():
    cert = os.environ.get("OKAY_TLS_CERT", "")
    key = os.environ.get("OKAY_TLS_KEY", "")
    if not cert and not key:
        return None
    if not cert or not key:
        fail("TLS needs both OKAY_TLS_CERT and OKAY_TLS_KEY; only one is set")
    ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    ctx.minimum_version = ssl.TLSVersion.TLSv1_2
    try:
        ctx.load_cert_chain(cert, key)
    except (OSError, ssl.SSLError) as e:
        fail("the TLS certificate %s and key %s: %s" % (cert, key, e))
    return ctx


def mac(key, message):
    return hmac.new(key, message.encode("utf-8"), hashlib.sha256).hexdigest()


def line(obj):
    return (json.dumps(obj, separators=(",", ":")) + "\n").encode("utf-8")


def condition(rid, kind, message):
    return line({"id": rid, "condition": {"kind": kind, "message": message}})


def authenticate(conn, rfile, secret, nonce):
    """the challenge, answered here; True when the host proved the secret"""
    while True:
        raw = rfile.readline()
        if not raw:
            return False
        if not raw.strip():
            continue
        try:
            req = json.loads(raw)
        except ValueError:
            conn.sendall(condition(None, "ValueError", "not a JSON request"))
            continue
        rid = req.get("id")
        if req.get("op") != "auth":
            conn.sendall(condition(rid, "PermissionError",
                                   "this worker requires hmac-sha256 authentication first"))
            continue
        nc = req.get("nonce") or ""
        got = req.get("mac") or ""
        want = mac(secret, "okay-wire client|%s|%s" % (nonce, nc))
        if not nc or not hmac.compare_digest(got.encode(), want.encode()):
            conn.sendall(condition(rid, "PermissionError",
                                   "authentication refused: the mac does not prove this worker's secret"))
            return False
        conn.sendall(line({"id": rid, "ok": {"mac": mac(secret, "okay-wire server|%s|%s" % (nonce, nc))}}))
        return True


def pump(read, write, done):
    try:
        while True:
            chunk = read(65536)
            if not chunk:
                break
            write(chunk)
    except (OSError, ValueError):
        pass
    finally:
        done()


def serve(conn, command, env, secret, ctx):
    worker = None
    try:
        if ctx is not None:
            try:
                conn = ctx.wrap_socket(conn, server_side=True)
            except (ssl.SSLError, OSError):
                return      # a client that does not speak TLS: nothing to relay
        try:
            worker = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, env=env)
        except OSError as e:
            conn.sendall(condition(None, "WorkerUnavailable", "the gateway could not start its worker: %s" % e))
            return
        hello = worker.stdout.readline()
        if not hello:
            return
        rfile = conn.makefile("rb")
        if secret is not None:
            h = json.loads(hello)
            nonce = secrets.token_hex(16)
            h["auth"] = {"scheme": "hmac-sha256", "nonce": nonce}
            conn.sendall(line(h))
            if not authenticate(conn, rfile, secret, nonce):
                return
        else:
            conn.sendall(hello)

        def to_worker(chunk):
            worker.stdin.write(chunk)
            worker.stdin.flush()

        def close_worker_input():
            try:
                worker.stdin.close()
            except OSError:
                pass

        def close_socket_output():
            try:
                conn.shutdown(socket.SHUT_WR)
            except OSError:
                pass

        up = threading.Thread(target=pump, args=(rfile.read1, to_worker, close_worker_input), daemon=True)
        down = threading.Thread(target=pump, args=(worker.stdout.read1, conn.sendall, close_socket_output), daemon=True)
        up.start()
        down.start()
        down.join()
        up.join(timeout=1)
    finally:
        if worker is not None and worker.poll() is None:
            worker.kill()
            worker.wait()
        try:
            conn.close()
        except OSError:
            pass


def main(argv):
    if "--" not in argv or "--listen" not in argv:
        fail("usage: gateway.py --listen HOST:PORT -- WORKER COMMAND...")
    command = argv[argv.index("--") + 1:]
    listen = argv[argv.index("--listen") + 1]
    if not command:
        fail("no worker command after --")
    host, _, port = listen.rpartition(":")
    secret = secret_from_env()
    ctx = tls_from_env()
    # the worker needs none of the gateway's own keys
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("OKAY_WIRE_SECRET", "OKAY_TLS_", "OKAY_LISTEN"))}
    srv = socket.create_server((host, int(port)))
    bound = srv.getsockname()
    sys.stdout.write(json.dumps({"listening": "%s:%d" % (bound[0], bound[1]), "tls": ctx is not None}) + "\n")
    sys.stdout.flush()
    while True:
        conn, _ = srv.accept()
        conn.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        threading.Thread(target=serve, args=(conn, command, env, secret, ctx), daemon=True).start()


if __name__ == "__main__":
    main(sys.argv[1:])
