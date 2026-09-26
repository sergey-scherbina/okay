// okay's TypeScript worker (specs/typescript.md, stage 1): the process
// okay-py's engine drives, speaking the same wire as its Python shim — one
// JSON request and one answer per line, the handshake first. Calls,
// callbacks (call), held objects and programs as data (done/perform/then).
//
// The modules it serves are named in OKAY_TS_MODULES ("name=path;..."),
// imported ONCE at start: a callback's nested request is served
// synchronously, and an import is not.

import * as fs from "node:fs";
import * as zlib from "node:zlib";
import { pathToFileURL } from "node:url";
import { hooks, OkayError, type Prog } from "./okay.ts";

const SHIM = 9;
const EXACT = 2 ** 53;

// ---- the wire -----------------------------------------------------------

let pending: Buffer = Buffer.alloc(0);

/** read more of stdin into `pending`; false at its end */
function fill(): boolean {
  const chunk = Buffer.alloc(65536);
  for (;;) {
    let n: number;
    try {
      n = fs.readSync(0, chunk, 0, chunk.length, null);
    } catch (e: any) {
      if (e && e.code === "EAGAIN") continue;
      if (e && e.code === "EOF") return false;
      throw e;
    }
    if (n === 0) return false;
    pending = Buffer.concat([pending, chunk.subarray(0, n)]);
    return true;
  }
}

/** exactly n bytes of stdin; null if it ends first */
function readBytes(n: number): Buffer | null {
  while (pending.length < n) if (!fill()) return null;
  const out = pending.subarray(0, n);
  pending = pending.subarray(n);
  return out;
}

/** the next line of stdin, synchronously; null at its end */
function readLine(): string | null {
  for (;;) {
    const nl = pending.indexOf(10);
    if (nl >= 0) {
      const line = pending.subarray(0, nl).toString("utf8");
      pending = pending.subarray(nl + 1);
      return line;
    }
    if (!fill()) {
      if (pending.length === 0) return null;
      const rest = pending.toString("utf8");
      pending = Buffer.alloc(0);
      return rest;
    }
  }
}

// ---- the wire's encoding (polyglot-one-wire stage 5a) --------------------
// JSON lines until the host configures otherwise; then FRAMES (a 4-byte
// big-endian length, then the message), each the same tree as JSON or CBOR,
// optionally raw-DEFLATEd. CBOR here is the wire's subset, with no package.

const mode = { format: "json", compress: "none" };
let switchTo: { format: string; compress: string } | null = null;
const framed = () => mode.format !== "json" || mode.compress !== "none";

function cborHead(out: number[], major: number, n: number): void {
  const m = major << 5;
  if (n < 24) out.push(m | n);
  else if (n < 0x100) out.push(m | 24, n);
  else if (n < 0x10000) out.push(m | 25, n >> 8, n & 0xff);
  else if (n < 0x100000000) out.push(m | 26, (n >>> 24) & 0xff, (n >>> 16) & 0xff, (n >>> 8) & 0xff, n & 0xff);
  else {
    const b = Buffer.alloc(8);
    b.writeBigUInt64BE(BigInt(n));
    out.push(m | 27, ...b);
  }
}

function cborEnc(out: number[], v: any): void {
  if (v === null || v === undefined) out.push(0xf6);
  else if (v === true) out.push(0xf5);
  else if (v === false) out.push(0xf4);
  else if (typeof v === "number") {
    if (Number.isInteger(v) && Math.abs(v) < 2 ** 53) {
      if (v >= 0) cborHead(out, 0, v); else cborHead(out, 1, -1 - v);
    } else {
      const b = Buffer.alloc(8);
      b.writeDoubleBE(v);
      out.push(0xfb, ...b);
    }
  } else if (typeof v === "string") {
    const b = Buffer.from(v, "utf8");
    cborHead(out, 3, b.length);
    out.push(...b);
  } else if (Array.isArray(v)) {
    cborHead(out, 4, v.length);
    for (const x of v) cborEnc(out, x);
  } else if (typeof v === "object") {
    const ks = Object.keys(v);
    cborHead(out, 5, ks.length);
    for (const k of ks) { cborEnc(out, k); cborEnc(out, v[k]); }
  } else throw new Error(`a ${typeof v} does not encode as CBOR`);
}

function cborDec(b: Buffer, at: { i: number }): any {
  if (at.i >= b.length) throw new Error("a CBOR message ended early (cut short?)");
  const ib = b[at.i++];
  const major = ib >> 5, info = ib & 0x1f;
  const take = (n: number): Buffer => {
    if (at.i + n > b.length) throw new Error("a CBOR message ended early (cut short?)");
    const s = b.subarray(at.i, at.i + n);
    at.i += n;
    return s;
  };
  if (major === 7) {
    if (info === 20) return false;
    if (info === 21) return true;
    if (info === 22 || info === 23) return null;
    if (info === 25) {
      const h = take(2).readUInt16BE(0);
      const exp = (h >> 10) & 0x1f, mant = h & 0x3ff;
      const v = exp === 0 ? mant * 2 ** -24 : exp === 31 ? (mant ? NaN : Infinity) : (mant + 1024) * 2 ** (exp - 25);
      return h & 0x8000 ? -v : v;
    }
    if (info === 26) return take(4).readFloatBE(0);
    if (info === 27) return take(8).readDoubleBE(0);
    throw new Error(`CBOR: simple value ${info} is not in the wire's subset`);
  }
  let n: number;
  if (info < 24) n = info;
  else if (info === 24) n = take(1)[0];
  else if (info === 25) n = take(2).readUInt16BE(0);
  else if (info === 26) n = take(4).readUInt32BE(0);
  else if (info === 27) n = Number(take(8).readBigUInt64BE(0));
  else throw new Error(`CBOR: an indefinite or reserved length (${info}) is not in the wire's subset`);
  switch (major) {
    case 0: return n;
    case 1: return -1 - n;
    case 3: return take(n).toString("utf8");
    case 4: { const xs = []; for (let j = 0; j < n; j++) xs.push(cborDec(b, at)); return xs; }
    case 5: {
      const m: any = {};
      for (let j = 0; j < n; j++) {
        const k = cborDec(b, at);
        if (typeof k !== "string") throw new Error("CBOR: a map key that is not text");
        m[k] = cborDec(b, at);
      }
      return m;
    }
  }
  throw new Error(`CBOR: major type ${major} (byte strings, tags) is not in the wire's subset`);
}

function encode(obj: unknown): Buffer {
  let data: Buffer;
  if (mode.format === "cbor") { const out: number[] = []; cborEnc(out, obj); data = Buffer.from(out); }
  else data = Buffer.from(JSON.stringify(obj), "utf8");
  return mode.compress === "deflate" ? zlib.deflateRawSync(data) : data;
}

function decode(data: Buffer): any {
  if (mode.compress === "deflate") data = zlib.inflateRawSync(data);
  if (mode.format === "cbor") {
    const at = { i: 0 };
    const v = cborDec(data, at);
    if (at.i !== data.length) throw new Error(`CBOR: ${data.length - at.i} bytes after the message`);
    return v;
  }
  return JSON.parse(data.toString("utf8"));
}

function reply(obj: unknown): void {
  if (framed()) {
    const data = encode(obj);
    const len = Buffer.alloc(4);
    len.writeUInt32BE(data.length);
    fs.writeSync(1, Buffer.concat([len, data]));
  } else fs.writeSync(1, JSON.stringify(obj) + "\n");
  // a configure takes effect AFTER its own answer
  if (switchTo) { mode.format = switchTo.format; mode.compress = switchTo.compress; switchTo = null; }
}

/** the next request, or null when the host is gone */
function readMsg(): any | null {
  for (;;) {
    if (framed()) {
      const len = readBytes(4);
      if (len === null) return null;
      const data = readBytes(len.readUInt32BE(0));
      if (data === null) return null;
      return decode(data);
    }
    const line = readLine();
    if (line === null) return null;
    if (line.trim() !== "") return JSON.parse(line);
  }
}

// ---- values -------------------------------------------------------------

const held = new Map<number, any>();
let nextRef = 0;

function hold(obj: any): unknown {
  nextRef += 1;
  held.set(nextRef, obj);
  const type = obj === null || obj === undefined ? "null" : (obj.constructor?.name ?? typeof obj);
  return { t: "ref", id: nextRef, type };
}

function heldAt(id: number): any {
  if (!held.has(id)) throw new Error(`ref ${id} is not held by this process (released, or held by a process that is gone)`);
  return held.get(id);
}

function enc(v: any): unknown {
  if (v === null || v === undefined) return null;
  if (typeof v === "boolean" || typeof v === "string") return v;
  if (typeof v === "number") {
    if (Number.isNaN(v)) return { t: "nan" };
    if (Number.isInteger(v) && Math.abs(v) >= EXACT) return { t: "int", v: BigInt(v).toString() };
    return v;
  }
  if (typeof v === "bigint") return v > -BigInt(EXACT) && v < BigInt(EXACT) ? Number(v) : { t: "int", v: v.toString() };
  if (v instanceof Uint8Array) return { t: "bytes", b64: Buffer.from(v).toString("base64") };
  if (Array.isArray(v)) return v.map(enc);
  if (typeof v === "object") return { t: "dict", kv: Object.entries(v).map(([k, x]) => [k, enc(x)]) };
  throw new TypeError(`cannot encode a ${typeof v} for the wire`);
}

function dec(v: any): any {
  if (Array.isArray(v)) return v.map(dec);
  if (v !== null && typeof v === "object") {
    switch (v.t) {
      case "nan": return NaN;
      case "f": return v.v;
      case "int": return BigInt(v.v);
      case "bytes": return new Uint8Array(Buffer.from(v.b64, "base64"));
      case "dict": return Object.fromEntries(v.kv.map(([k, x]: [string, any]) => [k, dec(x)]));
      case "ref": return heldAt(v.id);
      case "na": return null;   // R's typed NA: TypeScript has one absence
      case "frame": return decFrame(v);
      default: throw new TypeError(`unknown tagged value: ${v.t}`);
    }
  }
  return v;
}

/** a record of columns as a frame on the wire (v1's per-cell pairs, which
 * every host reads) */
function encFrame(v: any): unknown {
  if (v === null || typeof v !== "object" || Array.isArray(v))
    throw new TypeError("a table function must answer a record of columns");
  return { t: "frame", cols: Object.entries(v).map(([k, xs]) => [k, (xs as unknown[]).map(enc)]) };
}

/** a frame off the wire as a record of columns: the COLUMNAR shape (v2,
 * foreign-one-value: a type per column, plain values, absences as index
 * lists) or v1's [name, cells] pairs */
function decFrame(v: any): Record<string, unknown[]> {
  const out: Record<string, unknown[]> = {};
  for (const c of v.cols) {
    if (Array.isArray(c)) { out[c[0]] = c[1].map(dec); continue; }
    if (c.cells !== undefined) { out[c.name] = c.cells.map(dec); continue; }
    const xs: unknown[] = [...c.values];
    for (const i of c.na ?? []) xs[i] = null;
    for (const i of c.nan ?? []) xs[i] = NaN;
    out[c.name] = xs;
  }
  return out;
}

// ---- modules ------------------------------------------------------------

const modules: Record<string, any> = {};

function resolve(fn: string): any {
  const at = fn.indexOf(":");
  if (at < 0) throw new Error(`a function is addressed as module:name, got '${fn}'`);
  const mod = modules[fn.slice(0, at)];
  if (mod === undefined) throw new Error(`no module named '${fn.slice(0, at)}' in this worker`);
  let obj: any = mod;
  for (const part of fn.slice(at + 1).split(".")) {
    obj = obj?.[part];
    if (obj === undefined) throw new Error(`module '${fn.slice(0, at)}' has no '${fn.slice(at + 1)}'`);
  }
  return obj;
}

// ---- callbacks: a direct function's okay_call is a node (foreign-one-program)

// the direct-style programs running now, innermost last: their run, the
// callbacks offered, and the id of the request each answers next
const direct: { run: number; offered: Set<string>; id: unknown }[] = [];
let nextAsk = 0;

hooks.call = (name: string, args: unknown[]): unknown => {
  const frame = direct[direct.length - 1];
  if (!frame) throw new Error(`call("${name}") outside a program okay started`);
  if (!frame.offered.has(name)) throw new Error(`call("${name}"): this call was offered ${JSON.stringify([...frame.offered].sort())}`);
  nextAsk += 1;
  const k = nextAsk;
  reply({ id: frame.id, ok: { perform: name, args: args.map(enc), k, once: true } });
  for (;;) {
    const req = readMsg();
    if (req === null) process.exit(0);
    if (req.op === "continue" && req.run === frame.run && req.k === k) {
      frame.id = req.id;
      if (req.condition) throw new OkayError(req.condition.kind, req.condition.message);
      return dec(req.answer);
    }
    const nested = serve(req);
    reply(nested instanceof Promise
      ? { id: req.id, condition: { kind: "Error", message: "an async function cannot answer inside a callback" } }
      : nested);
  }
};

// ---- programs as data ----------------------------------------------------

const runs = new Map<number, Map<number, (x: any) => Prog<any>>>();
let nextKont = 0;

function node(run: number, p: any): unknown {
  if (p && p.tag === "done") return { done: enc(p.value) };
  if (p && p.tag === "perform" && typeof p.k === "function") {
    nextKont += 1;
    if (!runs.has(run)) runs.set(run, new Map());
    runs.get(run)!.set(nextKont, p.k);
    return { perform: p.name, args: p.args.map(enc), k: nextKont };
  }
  throw new TypeError("a program answers done(v) or perform(name, ...)");
}

// ---- serving -------------------------------------------------------------

function condition(id: unknown, e: any): unknown {
  const kind = e instanceof OkayError ? "OkayError" : (e?.name ?? "Error");
  return { id, condition: { kind, message: String(e?.message ?? e) } };
}

function settle(id: unknown, out: any, ok: (v: any) => unknown): unknown {
  if (out instanceof Promise) return out.then((v) => ({ id, ok: ok(v) }), (e) => condition(id, e));
  return { id, ok: ok(out) };
}

function serve(req: any): unknown {
  const id = req.id;
  try {
    const args = (req.args ?? []).map(dec);
    switch (req.op) {
      case "call": {
        // THE call (foreign-one-held): a name, or a held object's method or
        // attribute; `held` keeps the answer here and answers its ref
        const at = req.fn;
        const out = typeof at === "string" ? resolve(at)(...args)
          : at.method !== undefined ? heldAt(at.ref)[at.method](...args)
          : at.attr !== undefined ? heldAt(at.ref)[at.attr]
          : (() => { throw new Error(`a call's address is a name, a method or an attribute, got ${JSON.stringify(at)}`); })();
        // `table` (foreign-one-protocol): the answer is a table — a record
        // of columns, as the first argument arrived
        return settle(id, out, req.held ? hold : req.table ? encFrame : enc);
      }
      case "program": {
        // ONE program protocol (foreign-one-program): a program as data
        // (done/perform), or ordinary code whose okay_call's are `once` nodes
        const frame = { run: req.run, offered: new Set<string>(req.callbacks ?? []), id };
        direct.push(frame);
        let out: any;
        try {
          out = resolve(req.fn)(...args);
        } catch (e) {
          return condition(frame.id, e);
        } finally {
          direct.pop();
        }
        if (out && (out.tag === "done" || out.tag === "perform")) return { id: frame.id, ok: node(req.run, out) };
        return settle(frame.id, out, (v) => ({ done: enc(v) }));
      }
      case "continue": {
        const k = runs.get(req.run)?.get(req.k);
        if (!k) throw new Error(`continuation ${req.k} of run ${req.run} is not held here (forgotten, continued once already, or another process)`);
        if (req.condition) throw new Error("a program as data is continued with an answer, not a condition");
        return { id, ok: node(req.run, k(dec(req.answer))) };
      }
      case "forget":
        runs.delete(req.run);
        return { id, ok: null };
      case "release":
        held.delete(req.ref);
        return { id, ok: null };
      case "configure": {
        if (req.format !== "json" && req.format !== "cbor")
          throw new Error(`this TypeScript worker speaks the formats json, cbor; not ${JSON.stringify(req.format)}`);
        if (req.compress !== "none" && req.compress !== "deflate")
          throw new Error(`this TypeScript worker speaks the compressions none, deflate; not ${JSON.stringify(req.compress)}`);
        switchTo = { format: req.format, compress: req.compress };
        return { id, ok: { format: req.format, compress: req.compress } };
      }
      default:
        throw new Error(`this TypeScript worker does not serve '${req.op}'`);
    }
  } catch (e) {
    return condition(id, e);
  }
}

async function main(): Promise<void> {
  for (const entry of (process.env.OKAY_TS_MODULES ?? "").split(";").filter((s) => s !== "")) {
    const at = entry.indexOf("=");
    modules[entry.slice(0, at)] = await import(pathToFileURL(entry.slice(at + 1)).href);
  }
  reply({ shim: SHIM, python: `node ${process.version}`, speaks: { format: ["json", "cbor"], compress: ["deflate"], frames: ["columnar"] } });
  for (;;) {
    const req = readMsg();
    if (req === null) break;
    const out = serve(req);
    reply(out instanceof Promise ? await out : out);
  }
}

main().catch((e) => {
  reply({ shim: SHIM, fatal: `okay ts worker: ${e?.message ?? e}` });
  process.exit(1);
});
