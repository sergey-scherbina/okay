//! okay for Rust (polyglot-one-wire, specs/polyglot-one-wire.md): Rust code
//! that performs okay's effects, served on okay's wire so a Scala program
//! calls it exactly as it calls Python, Haskell or Go.
//!
//! Two styles:
//!
//! - **Programs as data** ([`Prog`]): `done`, `perform`, `and_then`. A
//!   continuation is an `Rc<dyn Fn>`, callable again, so okay may resume it
//!   more than once (multi-shot: a `Choice` handler makes every branch).
//! - **Direct style** ([`Functions`]): ordinary Rust that calls an effect in
//!   the middle of a computation, `ctx.call("price_of", args) -> answer`,
//!   the operator's `okay_call(request) -> answer`. Answered once.
//!
//! [`Worker::handle`] is the protocol with no I/O (one request line in, one
//! answer line out); [`serve_stdio`], [`serve_tcp`] and [`main`] carry it
//! over a child process's pipes or a socket.

use serde_json::{json, Map, Number, Value as J};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::marker::PhantomData;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::rc::Rc;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;

/// the wire version this worker speaks; the host refuses any other
pub const SHIM_VERSION: i64 = 6;

// ------------------------------------------------------------------ values

/// a value on the okay wire
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    List(Vec<Value>),
    Dict(Vec<(String, Value)>),
}

const EXACT: i64 = 1 << 53;

fn enc(v: &Value) -> J {
    match v {
        Value::Null => J::Null,
        Value::Bool(b) => J::Bool(*b),
        Value::Int(n) if *n > -EXACT && *n < EXACT => J::Number((*n).into()),
        Value::Int(n) => json!({"t": "int", "v": n.to_string()}),
        Value::Float(f) if f.is_nan() => json!({"t": "nan"}),
        Value::Float(f) if f.fract() == 0.0 && f.abs() < 1e15 => json!({"t": "f", "v": f}),
        Value::Float(f) => Number::from_f64(*f).map(J::Number).unwrap_or(J::Null),
        Value::Str(s) => J::String(s.clone()),
        Value::List(xs) => J::Array(xs.iter().map(enc).collect()),
        Value::Dict(kv) => json!({"t": "dict", "kv": kv.iter().map(|(k, v)| json!([k, enc(v)])).collect::<Vec<_>>()}),
    }
}

fn dec(j: &J) -> Value {
    match j {
        J::Null => Value::Null,
        J::Bool(b) => Value::Bool(*b),
        J::Number(n) => match n.as_i64() {
            Some(i) => Value::Int(i),
            None => Value::Float(n.as_f64().unwrap_or(f64::NAN)),
        },
        J::String(s) => Value::Str(s.clone()),
        J::Array(xs) => Value::List(xs.iter().map(dec).collect()),
        J::Object(m) => match m.get("t").and_then(|t| t.as_str()) {
            Some("nan") => Value::Float(f64::NAN),
            Some("f") => Value::Float(m.get("v").and_then(|v| v.as_f64()).unwrap_or(f64::NAN)),
            Some("int") => Value::Int(m.get("v").and_then(|v| v.as_str()).and_then(|s| s.parse().ok()).unwrap_or(0)),
            Some("dict") => Value::Dict(
                m.get("kv").and_then(|kv| kv.as_array()).map(|ps| {
                    ps.iter()
                        .filter_map(|p| {
                            let p = p.as_array()?;
                            Some((p.first()?.as_str()?.to_string(), dec(p.get(1)?)))
                        })
                        .collect()
                }).unwrap_or_default(),
            ),
            _ => {
                let mut kv: Vec<(String, Value)> = m.iter().map(|(k, v)| (k.clone(), dec(v))).collect();
                kv.sort_by(|a, b| a.0.cmp(&b.0));
                Value::Dict(kv)
            }
        },
    }
}

/// a Rust type that crosses the wire
pub trait Wire: Sized {
    fn to_value(&self) -> Value;
    fn from_value(v: &Value) -> Result<Self, String>;
}

impl Wire for Value {
    fn to_value(&self) -> Value { self.clone() }
    fn from_value(v: &Value) -> Result<Self, String> { Ok(v.clone()) }
}
impl Wire for i64 {
    fn to_value(&self) -> Value { Value::Int(*self) }
    fn from_value(v: &Value) -> Result<Self, String> {
        match v {
            Value::Int(n) => Ok(*n),
            Value::Float(f) if f.fract() == 0.0 => Ok(*f as i64),
            other => Err(format!("expected an integer, got {:?}", other)),
        }
    }
}
impl Wire for f64 {
    fn to_value(&self) -> Value { Value::Float(*self) }
    fn from_value(v: &Value) -> Result<Self, String> {
        match v {
            Value::Float(f) => Ok(*f),
            Value::Int(n) => Ok(*n as f64),
            other => Err(format!("expected a number, got {:?}", other)),
        }
    }
}
impl Wire for bool {
    fn to_value(&self) -> Value { Value::Bool(*self) }
    fn from_value(v: &Value) -> Result<Self, String> {
        match v { Value::Bool(b) => Ok(*b), other => Err(format!("expected a boolean, got {:?}", other)) }
    }
}
impl Wire for String {
    fn to_value(&self) -> Value { Value::Str(self.clone()) }
    fn from_value(v: &Value) -> Result<Self, String> {
        match v { Value::Str(s) => Ok(s.clone()), other => Err(format!("expected a string, got {:?}", other)) }
    }
}
impl<T: Wire> Wire for Vec<T> {
    fn to_value(&self) -> Value { Value::List(self.iter().map(Wire::to_value).collect()) }
    fn from_value(v: &Value) -> Result<Self, String> {
        match v {
            Value::List(xs) => xs.iter().map(T::from_value).collect(),
            other => Err(format!("expected a list, got {:?}", other)),
        }
    }
}
impl<T: Wire> Wire for Option<T> {
    fn to_value(&self) -> Value { self.as_ref().map(Wire::to_value).unwrap_or(Value::Null) }
    fn from_value(v: &Value) -> Result<Self, String> {
        match v { Value::Null => Ok(None), other => T::from_value(other).map(Some) }
    }
}

// ------------------------------------------------------- programs as data

/// a program as data: an answer, or a named operation and the function that
/// continues with its answer
pub enum Prog {
    Done(Value),
    Perform { name: String, args: Vec<Value>, k: Rc<dyn Fn(Value) -> Prog> },
}

/// a program that has answered `v`
pub fn done(v: impl Wire) -> Prog { Prog::Done(v.to_value()) }

/// ask okay to run the operation `name`; the program continues with its answer
pub fn perform(name: &str, args: Vec<Value>) -> Prog {
    Prog::Perform { name: name.to_string(), args, k: Rc::new(Prog::Done) }
}

impl Prog {
    /// this program, then `f` of its answer
    pub fn and_then(self, f: impl Fn(Value) -> Prog + 'static) -> Prog { self.bind(Rc::new(f)) }

    fn bind(self, f: Rc<dyn Fn(Value) -> Prog>) -> Prog {
        match self {
            Prog::Done(v) => f(v),
            Prog::Perform { name, args, k } => Prog::Perform {
                name,
                args,
                k: Rc::new(move |x| k(x).bind(f.clone())),
            },
        }
    }
}

/// one operation, typed by its answer: what `Rs.ops` in Scala generates from
/// the callbacks' Schemas
pub struct Op<A> {
    pub name: String,
    pub args: Vec<Value>,
    answer: PhantomData<A>,
}

impl<A: Wire> Op<A> {
    pub fn new(name: &str, args: Vec<Value>) -> Op<A> { Op { name: name.to_string(), args, answer: PhantomData } }
}

/// a program answering `A`
pub struct Program<A> {
    prog: Prog,
    answer: PhantomData<A>,
}

impl<A: Wire + 'static> Program<A> {
    /// the program that has answered `a`
    pub fn pure(a: A) -> Program<A> { Program { prog: done(a), answer: PhantomData } }

    /// this program, then `f` of its typed answer; an answer that does not
    /// decode panics, which the worker reports as a condition
    pub fn and_then<B: Wire + 'static>(self, f: impl Fn(A) -> Program<B> + 'static) -> Program<B> {
        Program {
            prog: self.prog.and_then(move |v| match A::from_value(&v) {
                Ok(a) => f(a).prog,
                Err(why) => panic!("an answer did not decode: {}", why),
            }),
            answer: PhantomData,
        }
    }

    /// the program for serving
    pub fn into_prog(self) -> Prog { self.prog }
}

/// perform one typed operation
pub fn send<A: Wire + 'static>(op: Op<A>) -> Program<A> {
    Program { prog: perform(&op.name, op.args), answer: PhantomData }
}

// ------------------------------------------------------------ direct style

/// a callback that failed in okay: its condition's kind and message
#[derive(Debug, Clone)]
pub struct OkayError {
    pub kind: String,
    pub message: String,
}

impl std::fmt::Display for OkayError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}: {}", self.kind, self.message) }
}

enum Event {
    Ask { cb: String, args: Vec<Value>, reply: Sender<Result<Value, OkayError>> },
    Done(Value),
    Fault(OkayError),
}

/// a direct-style call in progress: what `ctx.call` reaches okay through
pub struct Ctx {
    offered: Vec<String>,
    events: Sender<Event>,
}

impl Ctx {
    /// perform the okay callback `name`: the host runs it under the caller's
    /// handlers, and this returns its answer — `okay_call(request) -> answer`
    pub fn call(&self, name: &str, args: Vec<Value>) -> Result<Value, OkayError> {
        if !self.offered.iter().any(|n| n == name) {
            return Err(OkayError {
                kind: "LookupError".into(),
                message: format!("ctx.call({:?}): this call was offered {:?}", name, self.offered),
            });
        }
        let (tx, rx) = channel();
        self.events
            .send(Event::Ask { cb: name.to_string(), args, reply: tx })
            .map_err(|_| OkayError { kind: "WireError".into(), message: "the worker is gone".into() })?;
        rx.recv().unwrap_or_else(|_| Err(OkayError { kind: "WireError".into(), message: "the worker is gone".into() }))
    }

    /// `call`, typed by a generated operation
    pub fn call_op<A: Wire>(&self, op: Op<A>) -> Result<A, OkayError> {
        let v = self.call(&op.name, op.args)?;
        A::from_value(&v).map_err(|why| OkayError { kind: "DecodeError".into(), message: why })
    }
}

type DirectFn = Arc<dyn Fn(&Ctx, Vec<Value>) -> Result<Value, String> + Send + Sync>;

/// programs as data, served by name
pub type Programs = HashMap<String, Box<dyn Fn(Vec<Value>) -> Prog>>;
/// direct-style functions, served by name
pub type Functions = HashMap<String, DirectFn>;

/// a direct-style function for a `Functions` map
pub fn function<F>(f: F) -> DirectFn
where
    F: Fn(&Ctx, Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
{
    Arc::new(f)
}

// ------------------------------------------------------------------ the worker

struct Parked {
    id: J,
    events: Receiver<Event>,
    reply: Sender<Result<Value, OkayError>>,
}

/// the okay wire's protocol with no I/O: the programs and functions it serves,
/// the continuations and parked calls it holds
pub struct Worker {
    format: &'static str,   // "json" or "cbor" (stage 5a); json until a configure
    compress: &'static str, // "none" or "deflate"
    programs: Programs,
    functions: Functions,
    konts: HashMap<(i64, i64), Rc<dyn Fn(Value) -> Prog>>,
    next: i64,
    waiting: HashMap<i64, Parked>,
    asks: i64,
}

fn panic_message(p: Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = p.downcast_ref::<&str>() {
        s.to_string()
    } else if let Some(s) = p.downcast_ref::<String>() {
        s.clone()
    } else {
        "a panic".to_string()
    }
}

fn condition(id: &J, kind: &str, message: &str) -> J {
    json!({"id": id, "condition": {"kind": kind, "message": message}})
}

impl Worker {
    pub fn new(programs: Programs, functions: Functions) -> Worker {
        Worker { format: "json", compress: "none", programs, functions, konts: HashMap::new(), next: 0, waiting: HashMap::new(), asks: 0 }
    }

    /// the handshake line a worker speaks first
    pub fn hello() -> String {
        json!({"shim": SHIM_VERSION, "python": "rust",
               "speaks": {"format": ["json", "cbor"], "compress": ["deflate"]}}).to_string()
    }

    /// after a configure other than the defaults the wire carries frames
    pub fn framed(&self) -> bool { self.format != "json" || self.compress != "none" }

    fn node(&mut self, run: i64, p: Prog) -> J {
        match p {
            Prog::Done(v) => json!({"done": enc(&v)}),
            Prog::Perform { name, args, k } => {
                self.next += 1;
                self.konts.insert((run, self.next), k);
                json!({"perform": name, "args": args.iter().map(enc).collect::<Vec<_>>(), "k": self.next})
            }
        }
    }

    /// the next thing a direct-style call does: ask the host, or finish
    fn await_call(&mut self, id: J, events: Receiver<Event>) -> J {
        match events.recv() {
            Ok(Event::Ask { cb, args, reply }) => {
                self.asks += 1;
                let k = self.asks;
                let wire: Vec<J> = args.iter().map(enc).collect();
                self.waiting.insert(k, Parked { id, events, reply });
                json!({"ask": {"cb": cb, "args": wire, "k": k}})
            }
            Ok(Event::Done(v)) => json!({"id": id, "ok": enc(&v)}),
            Ok(Event::Fault(e)) => condition(&id, &e.kind, &e.message),
            Err(_) => condition(&id, "RustError", "the function's thread ended without an answer"),
        }
    }

    fn answer(&mut self, id: J, req: &Map<String, J>) -> J {
        let run = req.get("run").and_then(|r| r.as_i64()).unwrap_or(0);
        let args: Vec<Value> = req.get("args").and_then(|a| a.as_array()).map(|a| a.iter().map(dec).collect()).unwrap_or_default();
        match req.get("op").and_then(|o| o.as_str()) {
            Some("program") => {
                let fn_name = req.get("fn").and_then(|f| f.as_str()).unwrap_or("");
                let made = match self.programs.get(fn_name) {
                    None => return condition(&id, "LookupError", &format!("no program named '{}' in this worker", fn_name)),
                    Some(f) => catch_unwind(AssertUnwindSafe(|| f(args))),
                };
                match made {
                    Ok(p) => json!({"id": id, "ok": self.node(run, p)}),
                    Err(p) => condition(&id, "RustError", &panic_message(p)),
                }
            }
            Some("continue") => {
                let k = req.get("k").and_then(|k| k.as_i64()).unwrap_or(0);
                let f = match self.konts.get(&(run, k)) {
                    None => return condition(&id, "LookupError", &format!(
                        "continuation {} of run {} is not held here (forgotten, or another process)", k, run)),
                    Some(f) => f.clone(),
                };
                let answer = req.get("answer").map(dec).unwrap_or(Value::Null);
                match catch_unwind(AssertUnwindSafe(|| f(answer))) {
                    Ok(p) => json!({"id": id, "ok": self.node(run, p)}),
                    Err(p) => condition(&id, "RustError", &panic_message(p)),
                }
            }
            Some("forget") => {
                self.konts.retain(|(r, _), _| *r != run);
                json!({"id": id, "ok": null})
            }
            Some("start") => {
                let fn_name = req.get("fn").and_then(|f| f.as_str()).unwrap_or("").to_string();
                let f = match self.functions.get(&fn_name) {
                    None => return condition(&id, "LookupError", &format!("no function named '{}' in this worker", fn_name)),
                    Some(f) => f.clone(),
                };
                let offered: Vec<String> = req.get("callbacks").and_then(|c| c.as_array())
                    .map(|c| c.iter().filter_map(|n| n.as_str().map(String::from)).collect()).unwrap_or_default();
                let (tx, rx) = channel();
                std::thread::spawn(move || {
                    let ctx = Ctx { offered, events: tx.clone() };
                    let out = catch_unwind(AssertUnwindSafe(|| f(&ctx, args)));
                    let _ = tx.send(match out {
                        Ok(Ok(v)) => Event::Done(v),
                        Ok(Err(why)) => Event::Fault(OkayError { kind: "RustError".into(), message: why }),
                        Err(p) => Event::Fault(OkayError { kind: "RustError".into(), message: panic_message(p) }),
                    });
                });
                self.await_call(id, rx)
            }
            Some("resume") => {
                let k = req.get("k").and_then(|k| k.as_i64()).unwrap_or(0);
                let parked = match self.waiting.remove(&k) {
                    None => return condition(&id, "ValueError", &format!("resume {}: no call is waiting for it (resumed twice?)", k)),
                    Some(p) => p,
                };
                let answer = match req.get("condition").and_then(|c| c.as_object()) {
                    Some(c) => Err(OkayError {
                        kind: c.get("kind").and_then(|x| x.as_str()).unwrap_or("").into(),
                        message: c.get("message").and_then(|x| x.as_str()).unwrap_or("").into(),
                    }),
                    None => Ok(req.get("ok").map(dec).unwrap_or(Value::Null)),
                };
                let _ = parked.reply.send(answer);
                self.await_call(parked.id, parked.events)
            }
            Some("configure") => {
                let f = req.get("format").and_then(|x| x.as_str()).unwrap_or("");
                let c = req.get("compress").and_then(|x| x.as_str()).unwrap_or("");
                if f != "json" && f != "cbor" {
                    return condition(&id, "ValueError", &format!("this Rust worker speaks the formats json, cbor; not {:?}", f));
                }
                if c != "none" && c != "deflate" {
                    return condition(&id, "ValueError", &format!("this Rust worker speaks the compressions none, deflate; not {:?}", c));
                }
                json!({"id": id, "ok": {"format": f, "compress": c}})
            }
            other => condition(&id, "ValueError", &format!("this Rust worker serves programs and functions, not {:?}", other)),
        }
    }

    /// one message in the worker's current encoding in, one out in the same
    /// encoding; a configure takes effect AFTER its own answer
    pub fn handle_message(&mut self, msg: &[u8]) -> Vec<u8> {
        let (reply, configured) = match self.decode(msg) {
            Ok(J::Object(req)) => {
                let id = req.get("id").cloned().unwrap_or(J::Null);
                let reply = self.answer(id, &req);
                let configured = if req.get("op").and_then(|o| o.as_str()) == Some("configure") && reply.get("ok").is_some() {
                    Some((req.get("format").and_then(|x| x.as_str()) == Some("cbor"),
                          req.get("compress").and_then(|x| x.as_str()) == Some("deflate")))
                } else { None };
                (reply, configured)
            }
            Ok(_) => (condition(&J::Null, "ValueError", "a request is a map"), None),
            Err(why) => (condition(&J::Null, "ValueError", &why), None),
        };
        let out = self.encode(&reply);
        if let Some((cbor, deflate)) = configured {
            self.format = if cbor { "cbor" } else { "json" };
            self.compress = if deflate { "deflate" } else { "none" };
        }
        out
    }

    /// one request line in, one answer line out (the JSON-lines wire)
    pub fn handle(&mut self, line: &str) -> String {
        String::from_utf8_lossy(&self.handle_message(line.as_bytes())).into_owned()
    }

    fn decode(&self, msg: &[u8]) -> Result<J, String> {
        let raw;
        let bytes = if self.compress == "deflate" {
            let mut out = Vec::new();
            flate2::read::DeflateDecoder::new(msg).read_to_end(&mut out)
                .map_err(|e| format!("a DEFLATE message did not inflate: {}", e))?;
            raw = out;
            &raw[..]
        } else {
            msg
        };
        if self.format == "cbor" {
            let (v, rest) = cbor_decode(bytes)?;
            if !rest.is_empty() {
                return Err(format!("CBOR: {} bytes after the message", rest.len()));
            }
            Ok(v)
        } else {
            serde_json::from_slice(bytes).map_err(|_| "not a JSON request".to_string())
        }
    }

    fn encode(&self, reply: &J) -> Vec<u8> {
        let out = if self.format == "cbor" {
            let mut b = Vec::new();
            cbor_encode(&mut b, reply);
            b
        } else {
            reply.to_string().into_bytes()
        };
        if self.compress == "deflate" {
            let mut z = flate2::write::DeflateEncoder::new(Vec::new(), flate2::Compression::default());
            let _ = z.write_all(&out);
            z.finish().unwrap_or_default()
        } else {
            out
        }
    }
}

// ------------------------------------------------------------- CBOR (RFC 8949)

fn cbor_head(out: &mut Vec<u8>, major: u8, n: u64) {
    let m = major << 5;
    if n < 24 {
        out.push(m | n as u8);
    } else if n < 1 << 8 {
        out.push(m | 24);
        out.push(n as u8);
    } else if n < 1 << 16 {
        out.push(m | 25);
        out.extend_from_slice(&(n as u16).to_be_bytes());
    } else if n < 1 << 32 {
        out.push(m | 26);
        out.extend_from_slice(&(n as u32).to_be_bytes());
    } else {
        out.push(m | 27);
        out.extend_from_slice(&n.to_be_bytes());
    }
}

fn cbor_encode(out: &mut Vec<u8>, v: &J) {
    match v {
        J::Null => out.push(0xf6),
        J::Bool(b) => out.push(if *b { 0xf5 } else { 0xf4 }),
        J::Number(n) => {
            if let Some(i) = n.as_i64() {
                if i >= 0 { cbor_head(out, 0, i as u64) } else { cbor_head(out, 1, (-1 - i) as u64) }
            } else if let Some(u) = n.as_u64() {
                cbor_head(out, 0, u)
            } else {
                out.push(0xfb);
                out.extend_from_slice(&n.as_f64().unwrap_or(f64::NAN).to_bits().to_be_bytes());
            }
        }
        J::String(s) => {
            cbor_head(out, 3, s.len() as u64);
            out.extend_from_slice(s.as_bytes());
        }
        J::Array(xs) => {
            cbor_head(out, 4, xs.len() as u64);
            for x in xs {
                cbor_encode(out, x);
            }
        }
        J::Object(m) => {
            cbor_head(out, 5, m.len() as u64);
            for (k, x) in m {
                cbor_head(out, 3, k.len() as u64);
                out.extend_from_slice(k.as_bytes());
                cbor_encode(out, x);
            }
        }
    }
}

fn cbor_arg(info: u8, b: &[u8]) -> Result<(u64, &[u8]), String> {
    let n = match info {
        i if i < 24 => return Ok((i as u64, b)),
        24 => 1,
        25 => 2,
        26 => 4,
        27 => 8,
        other => return Err(format!("CBOR: an indefinite or reserved length ({}) is not in the wire's subset", other)),
    };
    if b.len() < n {
        return Err("a CBOR message ended early (cut short?)".into());
    }
    Ok((b[..n].iter().fold(0u64, |acc, c| (acc << 8) | *c as u64), &b[n..]))
}

fn half(h: u16) -> f64 {
    let exp = ((h >> 10) & 0x1f) as i32;
    let mant = (h & 0x3ff) as f64;
    let v = match exp {
        0 => mant * 2f64.powi(-24),
        31 => if mant == 0.0 { f64::INFINITY } else { f64::NAN },
        e => (mant + 1024.0) * 2f64.powi(e - 25),
    };
    if h & 0x8000 != 0 { -v } else { v }
}

fn num(f: f64) -> J { Number::from_f64(f).map(J::Number).unwrap_or(J::Null) }

fn cbor_decode(b: &[u8]) -> Result<(J, &[u8]), String> {
    let (&ib, b) = b.split_first().ok_or("a CBOR message ended early (cut short?)")?;
    let (major, info) = (ib >> 5, ib & 0x1f);
    if major == 7 {
        return match info {
            20 => Ok((J::Bool(false), b)),
            21 => Ok((J::Bool(true), b)),
            22 | 23 => Ok((J::Null, b)),
            25 => { let (x, r) = cbor_arg(info, b)?; Ok((num(half(x as u16)), r)) }
            26 => { let (x, r) = cbor_arg(info, b)?; Ok((num(f32::from_bits(x as u32) as f64), r)) }
            27 => { let (x, r) = cbor_arg(info, b)?; Ok((num(f64::from_bits(x)), r)) }
            other => Err(format!("CBOR: simple value {} is not in the wire's subset", other)),
        };
    }
    let (n, mut b) = cbor_arg(info, b)?;
    match major {
        0 => Ok((J::Number(n.into()), b)),
        1 => Ok((J::Number((-1 - n as i64).into()), b)),
        3 => {
            let n = n as usize;
            if b.len() < n {
                return Err("a CBOR string ended early (cut short?)".into());
            }
            let s = std::str::from_utf8(&b[..n]).map_err(|_| "CBOR: a text string that is not UTF-8")?;
            Ok((J::String(s.to_string()), &b[n..]))
        }
        4 => {
            let mut xs = Vec::new();
            for _ in 0..n {
                let (x, r) = cbor_decode(b)?;
                xs.push(x);
                b = r;
            }
            Ok((J::Array(xs), b))
        }
        5 => {
            let mut m = Map::new();
            for _ in 0..n {
                let (k, r) = cbor_decode(b)?;
                let (v, r) = cbor_decode(r)?;
                match k {
                    J::String(k) => { m.insert(k, v); }
                    _ => return Err("CBOR: a map key that is not text".into()),
                }
                b = r;
            }
            Ok((J::Object(m), b))
        }
        other => Err(format!("CBOR: major type {} (byte strings, tags) is not in the wire's subset", other)),
    }
}

// ------------------------------------------------------------------ transports

fn serve_lines(mut w: Worker, mut input: impl BufRead, mut output: impl Write) {
    let _ = writeln!(output, "{}", Worker::hello());
    let _ = output.flush();
    loop {
        if w.framed() {
            // frames: a 4-byte big-endian length, then the message (stage 5a)
            let mut n = [0u8; 4];
            if input.read_exact(&mut n).is_err() {
                return;
            }
            let mut msg = vec![0u8; u32::from_be_bytes(n) as usize];
            if input.read_exact(&mut msg).is_err() {
                return;
            }
            let reply = w.handle_message(&msg);
            let _ = output.write_all(&(reply.len() as u32).to_be_bytes());
            let _ = output.write_all(&reply);
            let _ = output.flush();
            continue;
        }
        let mut line = String::new();
        match input.read_line(&mut line) {
            Ok(0) | Err(_) => return,
            Ok(_) => {}
        }
        if line.trim().is_empty() {
            continue;
        }
        let _ = writeln!(output, "{}", w.handle(line.trim_end()));
        let _ = output.flush();
    }
}

/// serve on stdin/stdout: a child process
pub fn serve_stdio(make: fn() -> Worker) {
    let stdin = std::io::stdin();
    serve_lines(make(), stdin.lock(), std::io::stdout());
}

/// serve on a socket: another process, another machine. Each connection gets
/// a Worker of its own (made by `make`, on the connection's thread), and once
/// bound this prints {"listening": "host:port"} on stdout. PLAIN TCP,
/// unauthenticated: a trusted network, or TLS or SSH in front of it.
pub fn serve_tcp(addr: &str, make: fn() -> Worker) -> std::io::Result<()> {
    let listener = std::net::TcpListener::bind(addr)?;
    println!("{}", json!({"listening": listener.local_addr()?.to_string()}));
    std::io::stdout().flush()?;
    for stream in listener.incoming() {
        let stream = stream?;
        std::thread::spawn(move || {
            let _ = stream.set_nodelay(true);
            let reader = BufReader::new(match stream.try_clone() { Ok(s) => s, Err(_) => return });
            serve_lines(make(), reader, stream);
        });
    }
    Ok(())
}

/// serve on TCP when OKAY_LISTEN names an address, on stdin/stdout otherwise
pub fn main(make: fn() -> Worker) {
    match std::env::var("OKAY_LISTEN") {
        Ok(addr) if !addr.is_empty() => {
            if let Err(e) = serve_tcp(&addr, make) {
                eprintln!("{}", e);
                std::process::exit(1);
            }
        }
        _ => serve_stdio(make),
    }
}

// ------------------------------------------------------------------ in-process

/// the one Worker of a library loaded INTO the host's process (FFM, or a
/// WebAssembly module): what `export_worker!`'s `okay_exchange` serves.
///
/// SAFETY of `Send`: a Worker holds `Rc` continuations, which are not `Send`.
/// This wrapper is only ever reached through the `Mutex` in `export_worker!`,
/// so every touch of an `Rc` — clone, call, drop — happens under that lock,
/// one thread at a time; no `Rc` escapes the Worker (direct-style functions
/// run on threads of their own with `Send` channels only).
#[doc(hidden)]
pub struct InProcess(pub Worker);
unsafe impl Send for InProcess {}

/// one exchange in-process: an empty request answers the handshake
#[doc(hidden)]
pub fn exchange_in(global: &std::sync::Mutex<Option<InProcess>>, make: fn() -> Worker, req: &[u8]) -> Vec<u8> {
    if req.iter().all(|b| b.is_ascii_whitespace()) {
        return Worker::hello().into_bytes();
    }
    let mut guard = global.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let w = guard.get_or_insert_with(|| InProcess(make()));
    w.0.handle_message(req)
}

/// Export a worker IN-PROCESS: `okay_exchange(req, len, out_len) -> resp`
/// (one request line in, one answer line out; an empty request answers the
/// handshake), `okay_free(p, n)` for an answer, and `okay_alloc(n)` for a
/// host that must place a request in this module's memory (WebAssembly).
/// Loaded through FFM (a cdylib) or run by Chicory (wasm32-wasip1), the
/// Scala side drives it exactly as it drives a worker over a pipe.
#[macro_export]
macro_rules! export_worker {
    ($make:path) => {
        static OKAY_WORKER: std::sync::Mutex<Option<$crate::InProcess>> = std::sync::Mutex::new(None);

        #[no_mangle]
        pub extern "C" fn okay_exchange(req: *const u8, len: usize, out_len: *mut usize) -> *mut u8 {
            // SAFETY: the host hands `len` readable bytes at `req`, and a writable `usize` at `out_len`
            let bytes = if req.is_null() || len == 0 { &[][..] } else { unsafe { std::slice::from_raw_parts(req, len) } };
            let mut out = $crate::exchange_in(&OKAY_WORKER, $make, bytes).into_boxed_slice();
            unsafe { *out_len = out.len() };
            let p = out.as_mut_ptr();
            std::mem::forget(out);
            p
        }

        /// give back an answer (or an `okay_alloc`ed buffer) of `n` bytes
        #[no_mangle]
        pub extern "C" fn okay_free(p: *mut u8, n: usize) {
            if !p.is_null() {
                // SAFETY: `p` and `n` are exactly what okay_exchange or okay_alloc handed out
                unsafe { drop(Box::from_raw(std::ptr::slice_from_raw_parts_mut(p, n))) }
            }
        }

        /// `n` bytes of this module's memory, for a host to fill (n >= 1)
        #[no_mangle]
        pub extern "C" fn okay_alloc(n: usize) -> *mut u8 {
            let mut b = vec![0u8; n].into_boxed_slice();
            let p = b.as_mut_ptr();
            std::mem::forget(b);
            p
        }
    };
}
