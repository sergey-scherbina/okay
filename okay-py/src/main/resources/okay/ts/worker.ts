// okay's TypeScript worker (specs/typescript.md, stage 1): the process
// okay-py's engine drives, speaking the same wire as its Python shim — one
// JSON request and one answer per line, the handshake first. Calls,
// callbacks (call), held objects and programs as data (done/perform/then).
//
// The modules it serves are named in OKAY_TS_MODULES ("name=path;..."),
// imported ONCE at start: a callback's nested request is served
// synchronously, and an import is not.

import * as fs from "node:fs";
import { pathToFileURL } from "node:url";
import { hooks, OkayError, type Prog } from "./okay.ts";

const SHIM = 6;
const EXACT = 2 ** 53;

// ---- the wire -----------------------------------------------------------

let pending: Buffer = Buffer.alloc(0);

/** the next line of stdin, synchronously; null at its end */
function readLine(): string | null {
  for (;;) {
    const nl = pending.indexOf(10);
    if (nl >= 0) {
      const line = pending.subarray(0, nl).toString("utf8");
      pending = pending.subarray(nl + 1);
      return line;
    }
    const chunk = Buffer.alloc(65536);
    let n: number;
    try {
      n = fs.readSync(0, chunk, 0, chunk.length, null);
    } catch (e: any) {
      if (e && e.code === "EAGAIN") continue;
      if (e && e.code === "EOF") return null;
      throw e;
    }
    if (n === 0) {
      if (pending.length === 0) return null;
      const rest = pending.toString("utf8");
      pending = Buffer.alloc(0);
      return rest;
    }
    pending = Buffer.concat([pending, chunk.subarray(0, n)]);
  }
}

function reply(obj: unknown): void {
  fs.writeSync(1, JSON.stringify(obj) + "\n");
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
      case "frame": return Object.fromEntries(v.cols.map(([k, xs]: [string, any[]]) => [k, xs.map(dec)]));
      default: throw new TypeError(`unknown tagged value: ${v.t}`);
    }
  }
  return v;
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

// ---- callbacks (start / resume) ------------------------------------------

const offered: Set<string>[] = [];
let nextAsk = 0;

hooks.call = (name: string, args: unknown[]): unknown => {
  const mine = offered[offered.length - 1];
  if (!mine) throw new Error(`call("${name}") outside a call okay started with callbacks`);
  if (!mine.has(name)) throw new Error(`call("${name}"): this call was offered ${JSON.stringify([...mine].sort())}`);
  nextAsk += 1;
  const k = nextAsk;
  reply({ ask: { cb: name, args: args.map(enc), k } });
  for (;;) {
    const line = readLine();
    if (line === null) process.exit(0);
    if (line.trim() === "") continue;
    const req = JSON.parse(line);
    if (req.op === "resume" && req.k === k) {
      if (req.condition) throw new OkayError(req.condition.kind, req.condition.message);
      return dec(req.ok);
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
      case "call":
        return settle(id, resolve(req.fn)(...args), enc);
      case "start": {
        offered.push(new Set(req.callbacks ?? []));
        try {
          return settle(id, resolve(req.fn)(...args), enc);
        } finally {
          offered.pop();
        }
      }
      case "program":
        return { id, ok: node(req.run, resolve(req.fn)(...args)) };
      case "continue": {
        const k = runs.get(req.run)?.get(req.k);
        if (!k) throw new Error(`continuation ${req.k} of run ${req.run} is not held here (forgotten, or another process)`);
        return { id, ok: node(req.run, k(dec(req.answer))) };
      }
      case "forget":
        runs.delete(req.run);
        return { id, ok: null };
      case "hold":
        return settle(id, resolve(req.fn)(...args), hold);
      case "method": {
        const obj = heldAt(req.ref);
        const out = obj[req.name](...args);
        return settle(id, out, req.hold ? hold : enc);
      }
      case "attr":
        return { id, ok: enc(heldAt(req.ref)[req.name]) };
      case "release":
        held.delete(req.ref);
        return { id, ok: null };
      case "resume":
        throw new Error(`resume ${req.k}: no call is waiting for it (resumed twice?)`);
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
  reply({ shim: SHIM, python: `node ${process.version}` });
  for (;;) {
    const line = readLine();
    if (line === null) break;
    if (line.trim() === "") continue;
    const out = serve(JSON.parse(line));
    reply(out instanceof Promise ? await out : out);
  }
}

main().catch((e) => {
  reply({ shim: SHIM, fatal: `okay ts worker: ${e?.message ?? e}` });
  process.exit(1);
});
