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
use std::io::{BufRead, BufReader, Write};
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
        Worker { programs, functions, konts: HashMap::new(), next: 0, waiting: HashMap::new(), asks: 0 }
    }

    /// the handshake line a worker speaks first
    pub fn hello() -> String { json!({"shim": SHIM_VERSION, "python": "rust"}).to_string() }

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
            other => condition(&id, "ValueError", &format!("this Rust worker serves programs and functions, not {:?}", other)),
        }
    }

    /// one request line in, one answer line out
    pub fn handle(&mut self, line: &str) -> String {
        match serde_json::from_str::<J>(line) {
            Ok(J::Object(req)) => {
                let id = req.get("id").cloned().unwrap_or(J::Null);
                self.answer(id, &req).to_string()
            }
            _ => condition(&J::Null, "ValueError", "not a JSON request").to_string(),
        }
    }
}

// ------------------------------------------------------------------ transports

fn serve_lines(mut w: Worker, input: impl BufRead, mut output: impl Write) {
    let _ = writeln!(output, "{}", Worker::hello());
    let _ = output.flush();
    for line in input.lines() {
        let line = match line { Ok(l) => l, Err(_) => return };
        if line.trim().is_empty() {
            continue;
        }
        let _ = writeln!(output, "{}", w.handle(&line));
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
