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
//!   the middle of a computation: [`okay_call`]`(request) -> answer`, the
//!   same name in every language okay speaks (the operator's). Answered once.
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
use std::cell::RefCell;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

/// the wire version this worker speaks; the host refuses any other
/// 7: foreign-one-program — `start`/`resume` fold into `program`/`continue`;
/// 8: foreign-one-held — `hold`/`method`/`attr` fold into `call` (shared; no held objects here);
/// 9: foreign-one-protocol — `frame` folds into `call`; tables since
/// foreign-one-bulk (`Value::Table` argument and answer)
pub const SHIM_VERSION: i64 = 9;

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
    /// a TABLE (foreign-one-bulk): named columns in order; a table call's
    /// first argument arrives as one, and a function answering one answers
    /// a table
    Table(Vec<(String, Vec<Value>)>),
}

impl Value {
    /// the named column of a table
    pub fn col(&self, name: &str) -> Option<&Vec<Value>> {
        match self {
            Value::Table(cols) => cols.iter().find(|(n, _)| n == name).map(|(_, v)| v),
            _ => None,
        }
    }
}

/// a frame in the columnar shape this worker claims in its hello: a type per
/// column, plain values, the absences as index lists (or a column of cells,
/// where one column mixes kinds)
fn dec_frame(m: &Map<String, J>) -> Value {
    let cols = m.get("cols").and_then(|c| c.as_array()).cloned().unwrap_or_default();
    Value::Table(cols.iter().filter_map(|c| match c {
        J::Object(col) => {
            let name = col.get("name")?.as_str()?.to_string();
            if let Some(cells) = col.get("cells").and_then(|c| c.as_array()) {
                return Some((name, cells.iter().map(dec).collect()));
            }
            let doubles = col.get("type").and_then(|t| t.as_str()) == Some("d");
            let mut vs: Vec<Value> = col.get("values").and_then(|v| v.as_array()).map(|a| a.iter().map(|x| {
                let v = dec(x);
                match v { Value::Int(n) if doubles => Value::Float(n as f64), other => other }
            }).collect()).unwrap_or_default();
            for i in col.get("na").and_then(|a| a.as_array()).cloned().unwrap_or_default() {
                if let Some(i) = i.as_u64() { if (i as usize) < vs.len() { vs[i as usize] = Value::Null; } }
            }
            for i in col.get("nan").and_then(|a| a.as_array()).cloned().unwrap_or_default() {
                if let Some(i) = i.as_u64() { if (i as usize) < vs.len() { vs[i as usize] = Value::Float(f64::NAN); } }
            }
            Some((name, vs))
        }
        _ => None,
    }).collect())
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
        Value::Table(cols) => json!({"t": "frame", "cols": cols.iter()
            .map(|(k, vs)| json!([k, vs.iter().map(enc).collect::<Vec<_>>()])).collect::<Vec<_>>()}),
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
            Some("frame") => dec_frame(m),
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
    /// a stream's chunk, emitted (foreign-mux-duplex part 3)
    Chunk(Value),
    /// a chunk of the host's stream taken: one more credit (foreign-host-streams)
    Credit(i64),
}

/// a direct-style call in progress: what [`okay_call`] reaches okay through,
/// held by the thread the call runs on
struct Ctx {
    offered: Vec<String>,
    events: Sender<Event>,
    /// the stream this call feeds with okay_emit (foreign-mux-duplex part 3)
    stream: Option<Arc<OutStream>>,
    /// the host's stream into this call, read with okay_next (foreign-host-streams)
    input: Option<Arc<InStream>>,
}

/// one stream the HOST feeds into a call: its chunks as they come, and
/// whether it ended; each chunk the function takes grants the host one more
#[doc(hidden)]
pub struct InStream {
    id: i64,
    state: std::sync::Mutex<(std::collections::VecDeque<Value>, bool)>,
    more: std::sync::Condvar,
}

impl InStream {
    fn new(id: i64) -> InStream {
        InStream { id, state: std::sync::Mutex::new((std::collections::VecDeque::new(), false)), more: std::sync::Condvar::new() }
    }
    fn put(&self, v: Value) {
        let mut st = self.state.lock().unwrap_or_else(|p| p.into_inner());
        st.0.push_back(v);
        self.more.notify_all();
    }
    fn end(&self) {
        let mut st = self.state.lock().unwrap_or_else(|p| p.into_inner());
        st.1 = true;
        self.more.notify_all();
    }
}

/// okay_next(): the next chunk of the host's stream into this call
/// (foreign-host-streams), waiting until one comes; None at its end. Taking
/// one grants the host one more chunk of credit, so the host runs ahead of
/// this function by at most the credit it was given.
pub fn okay_next() -> Option<Value> {
    let (input, events) = CURRENT.with(|cur| {
        let cur = cur.borrow();
        let ctx = cur.as_ref()?;
        Some((ctx.input.clone()?, ctx.events.clone()))
    })?;
    let v = {
        let mut st = input.state.lock().unwrap_or_else(|p| p.into_inner());
        while st.0.is_empty() && !st.1 {
            st = input.more.wait(st).unwrap_or_else(|p| p.into_inner());
        }
        st.0.pop_front()?
    };
    let _ = events.send(Event::Credit(input.id));
    Some(v)
}

/// one stream the far side drives: the credit the host granted and has not
/// yet seen used, and whether the host cancelled it
#[doc(hidden)]
pub struct OutStream {
    state: std::sync::Mutex<(i64, bool)>,
    more: std::sync::Condvar,
}

impl OutStream {
    fn new(credit: i64) -> OutStream { OutStream { state: std::sync::Mutex::new((credit, false)), more: std::sync::Condvar::new() } }
    fn grant(&self, k: i64) {
        let mut st = self.state.lock().unwrap_or_else(|p| p.into_inner());
        st.0 += k;
        self.more.notify_all();
    }
    fn cancel(&self) {
        let mut st = self.state.lock().unwrap_or_else(|p| p.into_inner());
        st.1 = true;
        self.more.notify_all();
    }
}

/// okay_emit(chunk): one chunk of the stream this call feeds (foreign-mux-
/// duplex part 3), waiting while the host has granted no credit — the far
/// side runs ahead of its consumer by at most the credit. An error when the
/// host cancelled the stream (stop producing), or when this call is no stream.
pub fn okay_emit(chunk: impl Wire) -> Result<(), OkayError> {
    let (stream, events) = CURRENT.with(|cur| {
        let cur = cur.borrow();
        let ctx = cur.as_ref()?;
        Some((ctx.stream.clone()?, ctx.events.clone()))
    }).ok_or_else(|| OkayError { kind: "ValueError".into(), message: "okay_emit outside a stream: this call was not opened as one".into() })?;
    {
        let mut st = stream.state.lock().unwrap_or_else(|p| p.into_inner());
        while st.0 == 0 && !st.1 {
            st = stream.more.wait(st).unwrap_or_else(|p| p.into_inner());
        }
        if st.1 {
            return Err(OkayError { kind: "Cancelled".into(), message: "the host cancelled this stream".into() });
        }
        st.0 -= 1;
    }
    events.send(Event::Chunk(chunk.to_value())).map_err(|_| gone())
}

thread_local! {
    static CURRENT: RefCell<Option<Ctx>> = const { RefCell::new(None) };
}

fn gone() -> OkayError { OkayError { kind: "WireError".into(), message: "the worker is gone".into() } }

/// `okay_call(request) -> answer`: perform an okay operation from ordinary
/// Rust. The host runs its callback under the CALLER's handlers (a
/// `Reader`, a `State`, ...), and this returns the answer, typed by the
/// generated operation (`ops::price_of(sku)`), or `Op::<Value>::new(name,
/// args)` untyped. Only inside a direct-style function okay started.
pub fn okay_call<A: Wire>(request: Op<A>) -> Result<A, OkayError> {
    let rx = CURRENT.with(|cur| -> Result<Receiver<Result<Value, OkayError>>, OkayError> {
        let cur = cur.borrow();
        let ctx = cur.as_ref().ok_or_else(|| OkayError {
            kind: "RuntimeError".into(),
            message: format!("okay_call({:?}) outside a program okay started", request.name),
        })?;
        if !ctx.offered.iter().any(|n| *n == request.name) {
            return Err(OkayError {
                kind: "LookupError".into(),
                message: format!("okay_call({:?}): this call was offered {:?}", request.name, ctx.offered),
            });
        }
        let (tx, rx) = channel();
        ctx.events
            .send(Event::Ask { cb: request.name.clone(), args: request.args.clone(), reply: tx })
            .map_err(|_| gone())?;
        Ok(rx)
    })?;
    let v = rx.recv().unwrap_or_else(|_| Err(gone()))?;
    A::from_value(&v).map_err(|why| OkayError { kind: "DecodeError".into(), message: why })
}

/// so a direct-style function writes `okay_call(op)?`
impl From<OkayError> for String {
    fn from(e: OkayError) -> String { e.to_string() }
}

type DirectFn = Arc<dyn Fn(Vec<Value>) -> Result<Value, String> + Send + Sync>;

/// programs as data, served by name
pub type Programs = HashMap<String, Box<dyn Fn(Vec<Value>) -> Prog>>;
/// direct-style functions, served by name
pub type Functions = HashMap<String, DirectFn>;

/// a direct-style function for a `Functions` map
pub fn function<F>(f: F) -> DirectFn
where
    F: Fn(Vec<Value>) -> Result<Value, String> + Send + Sync + 'static,
{
    Arc::new(f)
}

// ------------------------------------------------------------------ the worker

struct Parked {
    /// the call's events, when this worker waits on them itself; None when
    /// they are forwarded to the worker's loop (a multiplexed wire)
    events: Option<Receiver<Event>>,
    reply: Sender<Result<Value, OkayError>>,
    /// the call, in the loop's table of calls (a multiplexed wire)
    call: Option<u64>,
}

/// what a multiplexed worker's loop takes, in whatever order it comes
/// (foreign-mux-duplex): a request off the wire, a running call's event, a
/// call whose thread is gone, or the wire's end
enum Loop {
    Request(Vec<u8>),
    Event(u64, Event),
    Gone(u64),
    Closed,
}

/// a call answered when its event comes, not when its request did
struct Pending {
    id: J,
    run: i64,
    plain: bool,
    hold: bool,
    /// the stream it feeds: its chunks and its end go out as stream messages
    stream: Option<i64>,
    /// the host's stream into it, dropped when it ends
    input: Option<i64>,
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
    waiting: HashMap<(i64, i64), Parked>,
    asks: i64,
    // stage 5b (wire-auth): a worker with a secret answers nothing but an
    // auth until the host has proved it holds the same secret
    secret: Option<Vec<u8>>,
    nonce: String,
    authed: bool,
    closing: bool, // a refused auth: the connection ends after its answer
    // values kept for the host, by ref (foreign-held-values): a call made
    // `held` keeps its answer here, an argument that is a ref is the value
    // again, `release` drops it
    held: HashMap<i64, Value>,
    next_ref: i64,
    // the multiplexed wire (foreign-mux-duplex): where a call's events go,
    // and the calls answered when they come
    events_to: Option<Sender<Loop>>,
    calls: HashMap<u64, Pending>,
    next_call: u64,
    /// the streams it drives, by the host's id (part 3)
    streams: HashMap<i64, Arc<OutStream>>,
    /// the streams the host feeds it (foreign-host-streams)
    inputs: HashMap<i64, Arc<InStream>>,
}

/// HMAC-SHA256 (RFC 2104) of `message` under `key`, as lower-case hex
fn mac_hex(key: &[u8], message: &str) -> String {
    use sha2::{Digest, Sha256};
    let mut k = [0u8; 64];
    if key.len() > 64 {
        k[..32].copy_from_slice(&Sha256::digest(key));
    } else {
        k[..key.len()].copy_from_slice(key);
    }
    let (mut ipad, mut opad) = ([0x36u8; 64], [0x5cu8; 64]);
    for i in 0..64 {
        ipad[i] ^= k[i];
        opad[i] ^= k[i];
    }
    let inner = Sha256::new().chain_update(ipad).chain_update(message.as_bytes()).finalize();
    let outer = Sha256::new().chain_update(opad).chain_update(inner).finalize();
    outer.iter().map(|b| format!("{:02x}", b)).collect()
}

/// equal in constant time: a byte-by-byte compare leaks how much of a guess was right
fn same(a: &str, b: &str) -> bool {
    a.len() == b.len() && a.bytes().zip(b.bytes()).fold(0u8, |acc, (x, y)| acc | (x ^ y)) == 0
}

/// the wire secret a TCP server authenticates with: OKAY_WIRE_SECRET, or the
/// contents of the file OKAY_WIRE_SECRET_FILE names (a trailing newline
/// dropped); None when neither is set
pub fn secret_from_env() -> Result<Option<Vec<u8>>, String> {
    if let Ok(s) = std::env::var("OKAY_WIRE_SECRET") {
        if !s.is_empty() {
            return Ok(Some(s.into_bytes()));
        }
    }
    if let Ok(f) = std::env::var("OKAY_WIRE_SECRET_FILE") {
        if !f.is_empty() {
            let mut b = std::fs::read(&f).map_err(|e| format!("the wire secret's file {}: {}", f, e))?;
            while matches!(b.last(), Some(b'\n') | Some(b'\r')) {
                b.pop();
            }
            if b.is_empty() {
                return Err(format!("the wire secret's file {} is empty", f));
            }
            return Ok(Some(b));
        }
    }
    Ok(None)
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
        Worker { format: "json", compress: "none", programs, functions, konts: HashMap::new(), next: 0, waiting: HashMap::new(), asks: 0,
                 secret: None, nonce: String::new(), authed: false, closing: false, held: HashMap::new(), next_ref: 0,
                 events_to: None, calls: HashMap::new(), next_call: 0, streams: HashMap::new(), inputs: HashMap::new() }
    }

    /// a worker that answers only after a mutual HMAC-SHA256 challenge
    pub fn with_secret(mut self, secret: Option<Vec<u8>>) -> Worker {
        self.secret = secret;
        self
    }

    /// this worker's handshake line: `hello`'s, plus, for a worker with a
    /// secret, the challenge its host must answer (stage 5b)
    pub fn hello_line(&mut self) -> String { self.hello_line_claiming(false) }

    fn hello_line_claiming(&mut self, mux: bool) -> String {
        if self.secret.is_none() {
            return Worker::hello_claiming(mux);
        }
        let mut b = [0u8; 16];
        std::fs::File::open("/dev/urandom").and_then(|mut f| f.read_exact(&mut b))
            .expect("okay: no randomness for the auth nonce (/dev/urandom)");
        self.nonce = b.iter().map(|x| format!("{:02x}", x)).collect();
        json!({"shim": SHIM_VERSION, "python": "rust",
               "speaks": {"format": ["json", "cbor"], "compress": ["deflate"], "frames": ["columnar"], "mux": mux},
               "auth": {"scheme": "hmac-sha256", "nonce": self.nonce}}).to_string()
    }

    /// a request while the host has not yet proved the secret: an auth,
    /// checked in constant time, or a refusal
    fn authenticate(&mut self, id: &J, req: &Map<String, J>) -> J {
        let secret = self.secret.clone().unwrap_or_default();
        if req.get("op").and_then(|o| o.as_str()) != Some("auth") {
            return condition(id, "PermissionError", "this worker requires hmac-sha256 authentication first");
        }
        let nc = req.get("nonce").and_then(|x| x.as_str()).unwrap_or("");
        let mac = req.get("mac").and_then(|x| x.as_str()).unwrap_or("");
        if nc.is_empty() || !same(mac, &mac_hex(&secret, &format!("okay-wire client|{}|{}", self.nonce, nc))) {
            self.closing = true;
            return condition(id, "PermissionError", "authentication refused: the mac does not prove this worker's secret");
        }
        self.authed = true;
        json!({"id": id, "ok": {"mac": mac_hex(&secret, &format!("okay-wire server|{}|{}", self.nonce, nc))}})
    }

    /// the handshake line a worker speaks first
    pub fn hello() -> String { Worker::hello_claiming(false) }

    /// the handshake line, claiming `mux` where the loop serving it answers
    /// requests as they finish (foreign-mux-duplex)
    fn hello_claiming(mux: bool) -> String {
        json!({"shim": SHIM_VERSION, "python": "rust",
               "speaks": {"format": ["json", "cbor"], "compress": ["deflate"], "frames": ["columnar"], "mux": mux}}).to_string()
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

    /// the next thing a direct-style call does: perform an okay operation (a
    /// node marked `once`, which the host continues), or finish — `done` as a
    /// program's node, or, for a plain `call`, the value itself
    fn await_call(&mut self, id: J, run: i64, events: Receiver<Event>, plain: bool) -> J {
        match events.recv() {
            Ok(Event::Ask { cb, args, reply }) => {
                self.asks += 1;
                let k = self.asks;
                let wire: Vec<J> = args.iter().map(enc).collect();
                let answer = json!({"id": id, "ok": {"perform": cb, "args": wire, "k": k, "once": true}});
                self.waiting.insert((run, k), Parked { events: Some(events), reply, call: None });
                answer
            }
            Ok(Event::Done(v)) if plain => json!({"id": id, "ok": enc(&v)}),
            Ok(Event::Done(v)) => json!({"id": id, "ok": {"done": enc(&v)}}),
            Ok(Event::Fault(e)) => condition(&id, &e.kind, &e.message),
            // okay_emit refuses outside a stream, so no chunk comes here
            Ok(Event::Chunk(_)) | Ok(Event::Credit(_)) => condition(&id, "RustError", "a stream event from a call that has no stream"),
            Err(_) => condition(&id, "RustError", "the function's thread ended without an answer"),
        }
    }

    /// a call on a MULTIPLEXED wire (foreign-mux-duplex): its events go to the
    /// worker's loop, tagged with the call, and it is answered when they come
    /// — J::Null here is "no reply yet"
    fn defer(&mut self, id: J, run: i64, events: Receiver<Event>, plain: bool, hold: bool, stream: Option<i64>) -> J {
        let to = match &self.events_to { Some(t) => t.clone(), None => return condition(&id, "RustError", "no loop to defer to") };
        self.next_call += 1;
        let call = self.next_call;
        self.calls.insert(call, Pending { id, run, plain, hold, stream, input: None });
        std::thread::spawn(move || {
            for e in events.iter() {
                if to.send(Loop::Event(call, e)).is_err() {
                    return;
                }
            }
            let _ = to.send(Loop::Gone(call));
        });
        J::Null
    }

    /// a deferred call's event: its reply, or None while it has none (it is
    /// parked on an okay_call, or already answered)
    fn on_event(&mut self, call: u64, e: Option<Event>) -> Option<J> {
        let p = self.calls.remove(&call)?;
        if let Some(s) = p.stream {
            // a stream's messages: its chunks, then its end or its condition
            return Some(match e {
                Some(Event::Chunk(v)) => {
                    self.calls.insert(call, p);
                    json!({"stream": s, "chunk": enc(&v)})
                }
                Some(Event::Credit(i)) => {
                    self.calls.insert(call, p);
                    json!({"stream": i, "credit": 1})
                }
                Some(Event::Ask { cb, reply, .. }) => {
                    let _ = reply.send(Err(OkayError { kind: "LookupError".into(),
                        message: format!("okay_call(\"{}\") in a stream, which offers no callbacks", cb) }));
                    self.calls.insert(call, p);
                    return None;
                }
                Some(Event::Done(_)) => {
                    self.streams.remove(&s);
                    if let Some(i) = p.input { self.inputs.remove(&i); }
                    json!({"stream": s, "end": true})
                }
                Some(Event::Fault(e)) => { self.streams.remove(&s); json!({"stream": s, "condition": {"kind": e.kind, "message": e.message}}) }
                None => { self.streams.remove(&s); json!({"stream": s, "condition": {"kind": "RustError", "message": "the stream's thread ended without an answer"}}) }
            });
        }
        Some(match e {
            Some(Event::Chunk(_)) | Some(Event::Credit(_)) => {
                self.calls.insert(call, p);
                return None;
            }
            Some(Event::Ask { cb, reply, .. }) if p.hold => {
                let _ = reply.send(Err(OkayError { kind: "LookupError".into(),
                    message: format!("okay_call(\"{}\") in a held call, which offers no callbacks", cb) }));
                self.calls.insert(call, p);
                return None;
            }
            Some(Event::Ask { cb, args, reply }) => {
                self.asks += 1;
                let k = self.asks;
                let wire: Vec<J> = args.iter().map(enc).collect();
                self.waiting.insert((p.run, k), Parked { events: None, reply, call: Some(call) });
                json!({"id": p.id, "ok": {"perform": cb, "args": wire, "k": k, "once": true}})
            }
            Some(Event::Done(v)) if p.hold => {
                self.next_ref += 1;
                let kind = match &v { Value::Table(_) => "table", Value::Dict(_) => "dict", Value::List(_) => "list", _ => "value" };
                self.held.insert(self.next_ref, v);
                json!({"id": p.id, "ok": {"t": "ref", "id": self.next_ref, "type": kind}})
            }
            Some(Event::Done(v)) if p.plain => json!({"id": p.id, "ok": enc(&v)}),
            Some(Event::Done(v)) => json!({"id": p.id, "ok": {"done": enc(&v)}}),
            Some(Event::Fault(e)) => condition(&p.id, &e.kind, &e.message),
            None => condition(&p.id, "RustError", "the function's thread ended without an answer"),
        })
    }

    /// a held call's answer: kept here, its ref answered; an okay_call in it
    /// is refused (a held call offers no callbacks)
    fn hold(&mut self, id: J, events: Receiver<Event>) -> J {
        loop {
            return match events.recv() {
                Ok(Event::Ask { cb, reply, .. }) => {
                    let _ = reply.send(Err(OkayError { kind: "LookupError".into(),
                        message: format!("okay_call(\"{}\") in a held call, which offers no callbacks", cb) }));
                    continue;
                }
                Ok(Event::Done(v)) => {
                    self.next_ref += 1;
                    let kind = match &v { Value::Table(_) => "table", Value::Dict(_) => "dict", Value::List(_) => "list", _ => "value" };
                    self.held.insert(self.next_ref, v);
                    json!({"id": id, "ok": {"t": "ref", "id": self.next_ref, "type": kind}})
                }
                Ok(Event::Fault(e)) => condition(&id, &e.kind, &e.message),
                // okay_emit refuses outside a stream, so no chunk comes here
                Ok(Event::Chunk(_)) | Ok(Event::Credit(_)) => continue,
                Err(_) => condition(&id, "RustError", "the function's thread ended without an answer"),
            };
        }
    }

    /// a direct-style function on a thread of its own, its okay_call's
    /// answered through the channel the worker reads
    fn begin(f: DirectFn, args: Vec<Value>, offered: Vec<String>) -> Receiver<Event> {
        Worker::begin_feeding(f, args, offered, None, None)
    }

    fn begin_feeding(f: DirectFn, args: Vec<Value>, offered: Vec<String>, stream: Option<Arc<OutStream>>,
                     input: Option<Arc<InStream>>) -> Receiver<Event> {
        let (tx, rx) = channel();
        std::thread::spawn(move || {
            CURRENT.with(|cur| *cur.borrow_mut() = Some(Ctx { offered, events: tx.clone(), stream, input }));
            let out = catch_unwind(AssertUnwindSafe(|| f(args)));
            let _ = tx.send(match out {
                Ok(Ok(v)) => Event::Done(v),
                Ok(Err(why)) => Event::Fault(OkayError { kind: "RustError".into(), message: why }),
                Err(p) => Event::Fault(OkayError { kind: "RustError".into(), message: panic_message(p) }),
            });
        });
        rx
    }

    /// the request's arguments, a held value back in each ref's place; a ref
    /// this worker does not hold is refused by name
    fn args(&self, req: &Map<String, J>) -> Result<Vec<Value>, String> {
        req.get("args").and_then(|a| a.as_array()).map(|a| a.iter().map(|j| match j {
            J::Object(m) if m.get("t").and_then(|t| t.as_str()) == Some("ref") => {
                let id = m.get("id").and_then(|i| i.as_i64()).unwrap_or(-1);
                self.held.get(&id).cloned().ok_or_else(|| format!(
                    "ref {} is not held by this worker (released, or held by a process that is gone)", id))
            }
            other => Ok(dec(other)),
        }).collect()).unwrap_or(Ok(Vec::new()))
    }

    fn answer(&mut self, id: J, req: &Map<String, J>) -> J {
        if self.secret.is_some() && !self.authed {
            return self.authenticate(&id, req);
        }
        let run = req.get("run").and_then(|r| r.as_i64()).unwrap_or(0);
        let args = match self.args(req) {
            Ok(a) => a,
            Err(why) => return condition(&id, "LookupError", &why),
        };
        match req.get("op").and_then(|o| o.as_str()) {
            Some("program") => {
                // ONE program protocol (foreign-one-program): a program as data,
                // or a direct-style function whose okay_call's are `once` nodes
                let fn_name = req.get("fn").and_then(|f| f.as_str()).unwrap_or("").to_string();
                if let Some(f) = self.programs.get(&fn_name) {
                    return match catch_unwind(AssertUnwindSafe(|| f(args))) {
                        Ok(p) => json!({"id": id, "ok": self.node(run, p)}),
                        Err(p) => condition(&id, "RustError", &panic_message(p)),
                    };
                }
                let f = match self.functions.get(&fn_name) {
                    None => return condition(&id, "LookupError", &format!("no program or function named '{}' in this worker", fn_name)),
                    Some(f) => f.clone(),
                };
                let offered: Vec<String> = req.get("callbacks").and_then(|c| c.as_array())
                    .map(|c| c.iter().filter_map(|n| n.as_str().map(String::from)).collect()).unwrap_or_default();
                let rx = Worker::begin(f, args, offered);
                if self.events_to.is_some() {
                    return self.defer(id, run, rx, false, false, None);
                }
                self.await_call(id, run, rx, false)
            }
            Some("call") => {
                // a direct-style function with no callbacks: its value
                let fn_name = req.get("fn").and_then(|f| f.as_str()).unwrap_or("").to_string();
                let f = match self.functions.get(&fn_name) {
                    None => return condition(&id, "LookupError", &format!("no function named '{}' in this worker", fn_name)),
                    Some(f) => f.clone(),
                };
                if let Some(s) = req.get("stream").and_then(|s| s.as_i64()) {
                    if self.events_to.is_none() {
                        return condition(&id, "NotStreaming", "this Rust worker is not on a multiplexed wire: it cannot drive a stream");
                    }
                    let st = Arc::new(OutStream::new(req.get("credit").and_then(|c| c.as_i64()).unwrap_or(1)));
                    self.streams.insert(s, st.clone());
                    // the host's stream into the call, when its head names one
                    let input = req.get("input").and_then(|i| i.get("id")).and_then(|i| i.as_i64()).map(|i| {
                        let is = Arc::new(InStream::new(i));
                        self.inputs.insert(i, is.clone());
                        is
                    });
                    let input_id = input.as_ref().map(|i| i.id);
                    let rx = Worker::begin_feeding(f, args, Vec::new(), Some(st), input);
                    // answered at once: the stream's messages follow as it makes them
                    let _ = self.defer(id.clone(), run, rx, true, false, Some(s));
                    if let Some(p) = self.calls.get_mut(&self.next_call) {
                        p.input = input_id;
                    }
                    return json!({"id": id, "ok": null});
                }
                let rx = Worker::begin(f, args, Vec::new());
                let held = req.get("held").and_then(|h| h.as_bool()) == Some(true);
                if self.events_to.is_some() {
                    return self.defer(id, run, rx, true, held, None);
                }
                if held {
                    return self.hold(id, rx);
                }
                self.await_call(id, run, rx, true)
            }
            Some("continue") => {
                let k = req.get("k").and_then(|k| k.as_i64()).unwrap_or(0);
                if let Some(parked) = self.waiting.remove(&(run, k)) {
                    let answer = match req.get("condition").and_then(|c| c.as_object()) {
                        Some(c) => Err(OkayError {
                            kind: c.get("kind").and_then(|x| x.as_str()).unwrap_or("").into(),
                            message: c.get("message").and_then(|x| x.as_str()).unwrap_or("").into(),
                        }),
                        None => Ok(req.get("answer").map(dec).unwrap_or(Value::Null)),
                    };
                    let _ = parked.reply.send(answer);
                    return match (parked.call, parked.events) {
                        // its events come to the loop: answered when the next one does
                        (Some(call), _) => {
                            self.calls.insert(call, Pending { id, run, plain: false, hold: false, stream: None, input: None });
                            J::Null
                        }
                        (None, Some(events)) => self.await_call(id, run, events, false),
                        (None, None) => condition(&id, "RustError", "a parked call with nowhere to wait"),
                    };
                }
                let f = match self.konts.get(&(run, k)) {
                    None => return condition(&id, "LookupError", &format!(
                        "continuation {} of run {} is not held here (forgotten, continued once already, or another process)", k, run)),
                    Some(f) => f.clone(),
                };
                let answer = req.get("answer").map(dec).unwrap_or(Value::Null);
                match catch_unwind(AssertUnwindSafe(|| f(answer))) {
                    Ok(p) => json!({"id": id, "ok": self.node(run, p)}),
                    Err(p) => condition(&id, "RustError", &panic_message(p)),
                }
            }
            Some("credit") => {
                if let Some(st) = req.get("stream").and_then(|s| s.as_i64()).and_then(|s| self.streams.get(&s)) {
                    st.grant(req.get("credit").and_then(|c| c.as_i64()).unwrap_or(0));
                }
                json!({"id": id, "ok": null})
            }
            Some("chunk") => {
                if let Some(is) = req.get("stream").and_then(|s| s.as_i64()).and_then(|s| self.inputs.get(&s)) {
                    is.put(req.get("chunk").map(dec).unwrap_or(Value::Null));
                }
                json!({"id": id, "ok": null})
            }
            Some("end") => {
                if let Some(is) = req.get("stream").and_then(|s| s.as_i64()).and_then(|s| self.inputs.get(&s)) {
                    is.end();
                }
                json!({"id": id, "ok": null})
            }
            Some("cancel") => {
                // the call's input ends with it: a function waiting in okay_next wakes
                if let Some(is) = req.get("stream").and_then(|s| s.as_i64()).and_then(|s| self.inputs.get(&-s)) {
                    is.end();
                }
                if let Some(st) = req.get("stream").and_then(|s| s.as_i64()).and_then(|s| self.streams.remove(&s)) {
                    st.cancel();
                }
                json!({"id": id, "ok": null})
            }
            Some("release") => {
                if let Some(r) = req.get("ref").and_then(|r| r.as_i64()) { self.held.remove(&r); }
                json!({"id": id, "ok": null})
            }
            Some("forget") => {
                self.konts.retain(|(r, _), _| *r != run);
                json!({"id": id, "ok": null})
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
        self.respond(msg).unwrap_or_default()
    }

    /// one message's reply, or None when it has none YET (a call deferred on
    /// a multiplexed wire, answered when its event comes)
    fn respond(&mut self, msg: &[u8]) -> Option<Vec<u8>> {
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
        if reply.is_null() {
            return None;
        }
        let out = self.encode(&reply);
        if let Some((cbor, deflate)) = configured {
            self.format = if cbor { "cbor" } else { "json" };
            self.compress = if deflate { "deflate" } else { "none" };
        }
        Some(out)
    }

    /// a reply that came late, once its call's event did (foreign-mux-duplex)
    fn late(&self, reply: &J) -> Vec<u8> { self.encode(reply) }

    /// a TABLE call in-process (foreign-arrow-ffm): the head a message in
    /// the wire's format, the table itself the first argument — imported from
    /// C Data by `exchange_table_in`, never text. Answers the reply and, when
    /// the function answered a table, that table: the reply then says
    /// `{"t": "cdata"}` and the table crosses back as C Data.
    pub fn handle_table(&mut self, msg: &[u8], table: Value) -> (Vec<u8>, Option<Value>) {
        let req = match self.decode(msg) {
            Ok(J::Object(req)) => req,
            Ok(_) => return (self.encode(&condition(&J::Null, "ValueError", "a request is a map")), None),
            Err(why) => return (self.encode(&condition(&J::Null, "ValueError", &why)), None),
        };
        let id = req.get("id").cloned().unwrap_or(J::Null);
        if self.secret.is_some() && !self.authed {
            let refused = self.authenticate(&id, &req);
            return (self.encode(&refused), None);
        }
        let fn_name = req.get("fn").and_then(|f| f.as_str()).unwrap_or("").to_string();
        let f = match self.functions.get(&fn_name) {
            None => return (self.encode(&condition(&id, "LookupError", &format!("no function named '{}' in this worker", fn_name))), None),
            Some(f) => f.clone(),
        };
        let mut args = vec![table];
        match self.args(&req) {
            Ok(rest) => args.extend(rest),
            Err(why) => return (self.encode(&condition(&id, "LookupError", &why)), None),
        }
        let events = Worker::begin(f, args, Vec::new());
        loop {
            let reply = match events.recv() {
                // a table call offers no callbacks: an okay_call in it is refused, and it runs on
                Ok(Event::Ask { cb, reply, .. }) => {
                    let _ = reply.send(Err(OkayError { kind: "LookupError".into(),
                        message: format!("okay_call(\"{}\") in a table call, which offers no callbacks", cb) }));
                    continue;
                }
                Ok(Event::Done(v @ Value::Table(_))) => return (self.encode(&json!({"id": id, "ok": {"t": "cdata"}})), Some(v)),
                Ok(Event::Done(v)) => json!({"id": id, "ok": enc(&v)}),
                Ok(Event::Fault(e)) => condition(&id, &e.kind, &e.message),
                Ok(Event::Chunk(_)) | Ok(Event::Credit(_)) => continue,
                Err(_) => condition(&id, "RustError", "the function's thread ended without an answer"),
            };
            return (self.encode(&reply), None);
        }
    }

    /// the reply for a table answer that C Data cannot carry (a column mixing
    /// kinds): the same table, as an ordinary answer on the wire
    pub fn answer_in_line(&self, msg: &[u8], table: &Value) -> Vec<u8> {
        let id = match self.decode(msg) { Ok(J::Object(req)) => req.get("id").cloned().unwrap_or(J::Null), _ => J::Null };
        self.encode(&json!({"id": id, "ok": enc(table)}))
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

/// one connection's two directions as ONE object: a TLS stream cannot be
/// split into a reader and a writer, so the loop reads through a buffer and
/// writes through the same stream underneath it
struct Duplex<S>(BufReader<S>);

impl<S: Read> Read for Duplex<S> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> { self.0.read(buf) }
}
impl<S: Read> BufRead for Duplex<S> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> { self.0.fill_buf() }
    fn consume(&mut self, n: usize) { self.0.consume(n) }
}
impl<S: Read + Write> Write for Duplex<S> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> { self.0.get_mut().write(buf) }
    fn flush(&mut self) -> std::io::Result<()> { self.0.get_mut().flush() }
}

fn serve_lines(mut w: Worker, mut io: impl BufRead + Write) {
    let _ = writeln!(io, "{}", w.hello_line());
    let _ = io.flush();
    loop {
        if w.framed() {
            // frames: a 4-byte big-endian length, then the message (stage 5a)
            let mut n = [0u8; 4];
            if io.read_exact(&mut n).is_err() {
                return;
            }
            let mut msg = vec![0u8; u32::from_be_bytes(n) as usize];
            if io.read_exact(&mut msg).is_err() {
                return;
            }
            let reply = w.handle_message(&msg);
            let _ = io.write_all(&(reply.len() as u32).to_be_bytes());
            let _ = io.write_all(&reply);
            let _ = io.flush();
            if w.closing {
                return;
            }
            continue;
        }
        let mut line = String::new();
        match io.read_line(&mut line) {
            Ok(0) | Err(_) => return,
            Ok(_) => {}
        }
        if line.trim().is_empty() {
            continue;
        }
        let _ = writeln!(io, "{}", w.handle(line.trim_end()));
        let _ = io.flush();
        if w.closing {
            return;
        }
    }
}

/// serve a link whose two directions are apart — stdin and stdout, a TCP
/// stream and its clone — MULTIPLEXED (foreign-mux-duplex): a reader thread
/// takes requests off the wire, each running call's events come to the same
/// loop, and every request is answered when it finishes, matched by its id.
/// The worker stays on this thread (its programs hold `Rc` continuations);
/// only bytes and events cross threads.
fn serve_split(mut w: Worker, mut reader: impl BufRead + Send + 'static, mut writer: impl Write) {
    let _ = writeln!(writer, "{}", w.hello_line_claiming(true));
    let _ = writer.flush();
    let (to, from) = channel::<Loop>();
    w.events_to = Some(to.clone());
    let framed = Arc::new(AtomicBool::new(false));
    let (switched, switch) = channel::<()>();
    {
        let framed = framed.clone();
        std::thread::spawn(move || loop {
            let msg = if framed.load(Ordering::SeqCst) {
                let mut n = [0u8; 4];
                if reader.read_exact(&mut n).is_err() {
                    let _ = to.send(Loop::Closed);
                    return;
                }
                let mut msg = vec![0u8; u32::from_be_bytes(n) as usize];
                if reader.read_exact(&mut msg).is_err() {
                    let _ = to.send(Loop::Closed);
                    return;
                }
                msg
            } else {
                let mut line = String::new();
                match reader.read_line(&mut line) {
                    Ok(0) | Err(_) => {
                        let _ = to.send(Loop::Closed);
                        return;
                    }
                    Ok(_) => {}
                }
                if line.trim().is_empty() {
                    continue;
                }
                line.trim_end().as_bytes().to_vec()
            };
            // a configure changes how the NEXT message is read: wait for it
            let configure = !framed.load(Ordering::SeqCst) && is_configure(&msg);
            if to.send(Loop::Request(msg)).is_err() {
                return;
            }
            if configure && switch.recv().is_err() {
                return;
            }
        });
    }
    let write = |writer: &mut dyn Write, out: &[u8], frame: bool| {
        if frame {
            let _ = writer.write_all(&(out.len() as u32).to_be_bytes());
            let _ = writer.write_all(out);
        } else {
            let _ = writer.write_all(out);
            let _ = writer.write_all(b"\n");
        }
        let _ = writer.flush();
    };
    for m in from.iter() {
        match m {
            Loop::Request(bytes) => {
                let was = w.framed();
                let configure = !was && is_configure(&bytes);
                if let Some(out) = w.respond(&bytes) {
                    write(&mut writer, &out, was);
                }
                if configure {
                    framed.store(w.framed(), Ordering::SeqCst);
                    let _ = switched.send(());
                }
                if w.closing {
                    return;
                }
            }
            Loop::Event(call, e) => {
                if let Some(reply) = w.on_event(call, Some(e)) {
                    let out = w.late(&reply);
                    write(&mut writer, &out, w.framed());
                }
            }
            Loop::Gone(call) => {
                if let Some(reply) = w.on_event(call, None) {
                    let out = w.late(&reply);
                    write(&mut writer, &out, w.framed());
                }
            }
            Loop::Closed => return,
        }
    }
}

/// whether a line-mode message is a configure (it changes the framing)
fn is_configure(msg: &[u8]) -> bool {
    serde_json::from_slice::<J>(msg).ok()
        .and_then(|j| j.get("op").and_then(|o| o.as_str()).map(|o| o == "configure")).unwrap_or(false)
}

/// serve on stdin/stdout: a child process
pub fn serve_stdio(make: fn() -> Worker) {
    serve_split(make(), BufReader::new(std::io::stdin()), std::io::stdout());
}

/// serve on a socket: another process, another machine. Each connection gets
/// a Worker of its own (made by `make`, on the connection's thread), and once
/// bound this prints {"listening": "host:port"} on stdout. With
/// OKAY_WIRE_SECRET (or OKAY_WIRE_SECRET_FILE) set, every connection must
/// pass a mutual HMAC-SHA256 challenge before anything else (stage 5b);
/// without TLS the traffic after it is still plain TCP.
pub fn serve_tcp(addr: &str, make: fn() -> Worker) -> std::io::Result<()> {
    let invalid = |e: String| std::io::Error::new(std::io::ErrorKind::InvalidInput, e);
    let secret = secret_from_env().map_err(invalid)?;
    let tls = tls_from_env().map_err(invalid)?;
    let listener = std::net::TcpListener::bind(addr)?;
    println!("{}", json!({"listening": listener.local_addr()?.to_string(), "tls": tls.is_some()}));
    std::io::stdout().flush()?;
    for stream in listener.incoming() {
        let stream = stream?;
        let secret = secret.clone();
        let tls = tls.clone();
        std::thread::spawn(move || {
            let _ = stream.set_nodelay(true);
            let w = make().with_secret(secret);
            match tls {
                // two handles on one socket: read on one thread, written on this one
                None => match stream.try_clone() {
                    Ok(reading) => serve_split(w, BufReader::new(reading), stream),
                    Err(_) => serve_lines(w, Duplex(BufReader::new(stream))),
                },
                Some(config) => serve_tls(w, config, stream),
            }
        });
    }
    Ok(())
}

/// the server's TLS configuration from OKAY_TLS_CERT and OKAY_TLS_KEY (PEM
/// files); None when neither is set (wire-tls)
#[cfg(feature = "tls")]
type TlsConfig = Arc<rustls::ServerConfig>;
#[cfg(not(feature = "tls"))]
type TlsConfig = ();

fn tls_paths() -> Result<Option<(String, String)>, String> {
    let cert = std::env::var("OKAY_TLS_CERT").unwrap_or_default();
    let key = std::env::var("OKAY_TLS_KEY").unwrap_or_default();
    match (cert.is_empty(), key.is_empty()) {
        (true, true) => Ok(None),
        (false, false) => Ok(Some((cert, key))),
        _ => Err("TLS needs both OKAY_TLS_CERT and OKAY_TLS_KEY; only one is set".into()),
    }
}

#[cfg(feature = "tls")]
fn tls_from_env() -> Result<Option<TlsConfig>, String> {
    use rustls_pki_types::{pem::PemObject, CertificateDer, PrivateKeyDer};
    let Some((cert, key)) = tls_paths()? else { return Ok(None) };
    let certs: Vec<CertificateDer<'static>> = CertificateDer::pem_file_iter(&cert)
        .map_err(|e| format!("the TLS certificate {}: {}", cert, e))?
        .collect::<Result<_, _>>()
        .map_err(|e| format!("the TLS certificate {}: {}", cert, e))?;
    let key = PrivateKeyDer::from_pem_file(&key).map_err(|e| format!("the TLS key {}: {}", key, e))?;
    let config = rustls::ServerConfig::builder_with_provider(Arc::new(rustls::crypto::ring::default_provider()))
        .with_safe_default_protocol_versions()
        .map_err(|e| e.to_string())?
        .with_no_client_auth()
        .with_single_cert(certs, key)
        .map_err(|e| format!("the TLS certificate {} and its key: {}", cert, e))?;
    Ok(Some(Arc::new(config)))
}

#[cfg(not(feature = "tls"))]
fn tls_from_env() -> Result<Option<TlsConfig>, String> {
    match tls_paths()? {
        None => Ok(None),
        Some(_) => Err("OKAY_TLS_CERT is set, but this worker was built without the okay crate's tls feature: \
                        build it with RustWorker.build(dir, features = Seq(\"tls\"))".into()),
    }
}

#[cfg(feature = "tls")]
fn serve_tls(w: Worker, config: TlsConfig, stream: std::net::TcpStream) {
    match rustls::ServerConnection::new(config) {
        Ok(conn) => serve_lines(w, Duplex(BufReader::new(rustls::StreamOwned::new(conn, stream)))),
        Err(e) => eprintln!("okay: TLS: {}", e),
    }
}

#[cfg(not(feature = "tls"))]
fn serve_tls(_: Worker, _: TlsConfig, _: std::net::TcpStream) {}

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

/// one TABLE exchange in-process (foreign-arrow-ffm): the head in `req`, the
/// table as the Arrow C Data Interface — imported (and released) here, the
/// function run, and a table answer exported to `schema_out`/`array_out`,
/// released by the host through the callback the structs carry
///
/// # Safety
/// `schema_in`/`array_in` are live C Data structs of a struct array, and
/// `schema_out`/`array_out` point at writable ones
#[doc(hidden)]
pub unsafe fn exchange_table_in(global: &std::sync::Mutex<Option<InProcess>>, make: fn() -> Worker, req: &[u8],
                         schema_in: *mut cdata::FFI_ArrowSchema, array_in: *mut cdata::FFI_ArrowArray,
                         schema_out: *mut cdata::FFI_ArrowSchema, array_out: *mut cdata::FFI_ArrowArray) -> Vec<u8> {
    let mut guard = global.lock().unwrap_or_else(|poisoned| poisoned.into_inner());
    let w = guard.get_or_insert_with(|| InProcess(make()));
    // SAFETY: the host hands two C Data structs it filled, and two it zeroed for the answer
    let table = match cdata::import(schema_in, array_in) {
        Ok(t) => t,
        Err(why) => return w.0.encode(&condition(&J::Null, "ValueError", &format!("a C Data table: {}", why))),
    };
    let (reply, answered) = w.0.handle_table(req, table);
    match answered {
        None => reply,
        Some(t) => match cdata::export(&t, schema_out, array_out) {
            Ok(()) => reply,
            Err(_) => w.0.answer_in_line(req, &t),
        },
    }
}

/// The Arrow C Data Interface (https://arrow.apache.org/docs/format/CDataInterface.html),
/// written out rather than taken from the `arrow` crate, which every worker
/// build would pay for: a table is a struct array whose children are its
/// columns — int64 `l`, float64 `g`, utf8 `u`, boolean `b`, null `n`, the
/// columns a frame makes — each with a validity bitmap.
pub mod cdata {
    use super::Value;
    use std::ffi::{c_char, c_void, CStr, CString};

    #[repr(C)]
    pub struct FFI_ArrowSchema {
        pub format: *const c_char,
        pub name: *const c_char,
        pub metadata: *const c_char,
        pub flags: i64,
        pub n_children: i64,
        pub children: *mut *mut FFI_ArrowSchema,
        pub dictionary: *mut FFI_ArrowSchema,
        pub release: Option<unsafe extern "C" fn(*mut FFI_ArrowSchema)>,
        pub private_data: *mut c_void,
    }

    #[repr(C)]
    pub struct FFI_ArrowArray {
        pub length: i64,
        pub null_count: i64,
        pub offset: i64,
        pub n_buffers: i64,
        pub n_children: i64,
        pub buffers: *mut *const c_void,
        pub children: *mut *mut FFI_ArrowArray,
        pub dictionary: *mut FFI_ArrowArray,
        pub release: Option<unsafe extern "C" fn(*mut FFI_ArrowArray)>,
        pub private_data: *mut c_void,
    }

    const NULLABLE: i64 = 2;

    unsafe fn valid(bits: *const u8, i: usize) -> bool {
        bits.is_null() || (*bits.add(i / 8) >> (i % 8)) & 1 == 1
    }

    /// read a table the host exported, then release it: the consumer's duty
    ///
    /// # Safety
    /// `schema` and `array` are live C Data structs of a struct array
    pub unsafe fn import(schema: *mut FFI_ArrowSchema, array: *mut FFI_ArrowArray) -> Result<Value, String> {
        let out = read(&*schema, &*array);
        if let Some(r) = (*array).release { r(array) }
        if let Some(r) = (*schema).release { r(schema) }
        out
    }

    unsafe fn read(schema: &FFI_ArrowSchema, array: &FFI_ArrowArray) -> Result<Value, String> {
        let fmt = CStr::from_ptr(schema.format).to_string_lossy();
        if fmt != "+s" {
            return Err(format!("a table is a struct array (+s), not {}", fmt));
        }
        let n = array.length as usize;
        let mut cols = Vec::with_capacity(schema.n_children as usize);
        for c in 0..schema.n_children as usize {
            let cs = &**schema.children.add(c);
            let ca = &**array.children.add(c);
            let name = if cs.name.is_null() { String::new() } else { CStr::from_ptr(cs.name).to_string_lossy().into_owned() };
            let off = ca.offset as usize;
            let buf = |i: usize| *ca.buffers.add(i) as *const u8;
            let nulls = if ca.n_buffers > 0 { buf(0) } else { std::ptr::null() };
            let f = CStr::from_ptr(cs.format).to_string_lossy();
            let mut vs = Vec::with_capacity(n);
            for i in 0..n {
                let j = off + i;
                vs.push(if f == "n" || !valid(nulls, j) { Value::Null } else {
                    match &*f {
                        "l" => Value::Int(*(buf(1) as *const i64).add(j)),
                        "g" => Value::Float(*(buf(1) as *const f64).add(j)),
                        "b" => Value::Bool((*buf(1).add(j / 8) >> (j % 8)) & 1 == 1),
                        "u" => {
                            let o = buf(1) as *const i32;
                            let (a, b) = (*o.add(j) as usize, *o.add(j + 1) as usize);
                            Value::Str(String::from_utf8_lossy(std::slice::from_raw_parts(buf(2).add(a), b - a)).into_owned())
                        }
                        other => return Err(format!("column '{}' is {}, not one of l g u b n", name, other)),
                    }
                });
            }
            cols.push((name, vs));
        }
        Ok(Value::Table(cols))
    }

    /// what a column of Values is on the wire, or None where it mixes kinds
    fn kind(vs: &[Value]) -> Option<&'static str> {
        let mut k = "n";
        for v in vs {
            k = match (k, v) {
                (_, Value::Null) => k,
                ("n", Value::Int(_)) | ("l", Value::Int(_)) => "l",
                ("n", Value::Float(_)) | ("g", Value::Float(_)) | ("l", Value::Float(_)) | ("g", Value::Int(_)) => "g",
                ("n", Value::Str(_)) | ("u", Value::Str(_)) => "u",
                ("n", Value::Bool(_)) | ("b", Value::Bool(_)) => "b",
                _ => return None,
            };
        }
        Some(k)
    }

    /// everything an exported struct points at, freed by its release
    struct Owned {
        _strings: Vec<CString>,
        _bytes: Vec<Vec<u8>>,
        _schemas: Vec<FFI_ArrowSchema>,
        _arrays: Vec<FFI_ArrowArray>,
        _schema_ptrs: Vec<*mut FFI_ArrowSchema>,
        _array_ptrs: Vec<*mut FFI_ArrowArray>,
        _buffer_ptrs: Vec<Vec<*const c_void>>,
    }

    unsafe extern "C" fn release_schema(s: *mut FFI_ArrowSchema) {
        if !(*s).private_data.is_null() { drop(Box::from_raw((*s).private_data as *mut Owned)) }
        (*s).release = None;
    }
    unsafe extern "C" fn release_array(a: *mut FFI_ArrowArray) {
        if !(*a).private_data.is_null() { drop(Box::from_raw((*a).private_data as *mut Owned)) }
        (*a).release = None;
    }
    /// a child's memory is its parent's; releasing it only marks it released
    unsafe extern "C" fn release_child_schema(s: *mut FFI_ArrowSchema) { (*s).release = None; }
    unsafe extern "C" fn release_child_array(a: *mut FFI_ArrowArray) { (*a).release = None; }

    fn schema(format: *const c_char, name: *const c_char, flags: i64) -> FFI_ArrowSchema {
        FFI_ArrowSchema { format, name, metadata: std::ptr::null(), flags, n_children: 0, children: std::ptr::null_mut(),
                          dictionary: std::ptr::null_mut(), release: Some(release_child_schema), private_data: std::ptr::null_mut() }
    }

    /// export a table into the two structs the host zeroed; Err where a column
    /// mixes kinds (the caller answers it on the wire instead)
    ///
    /// # Safety
    /// `schema` and `array` point at writable C Data structs
    pub unsafe fn export(t: &Value, schema_out: *mut FFI_ArrowSchema, array_out: *mut FFI_ArrowArray) -> Result<(), String> {
        let cols = match t { Value::Table(c) => c, _ => return Err("not a table".into()) };
        let kinds = cols.iter().map(|(n, vs)| kind(vs).ok_or_else(|| format!("column '{}' mixes kinds", n)))
            .collect::<Result<Vec<_>, _>>()?;
        let rows = cols.first().map(|(_, v)| v.len()).unwrap_or(0);
        let mut strings = Vec::new();
        let mut bytes: Vec<Vec<u8>> = Vec::new();
        let mut schemas = Vec::new();
        let mut arrays = Vec::new();
        let mut buffer_ptrs = Vec::new();
        let top = CString::new("+s").unwrap();
        let mut child_formats = Vec::new();
        for ((name, vs), k) in cols.iter().zip(&kinds) {
            let f = CString::new(*k).unwrap();
            let nm = CString::new(name.as_str()).unwrap_or_default();
            schemas.push(schema(f.as_ptr(), nm.as_ptr(), NULLABLE));
            child_formats.push(f);
            strings.push(nm);
            // validity: one bit a row, 1 = present
            let mut bits = vec![0u8; rows.div_ceil(8)];
            let mut nulls = 0i64;
            for (i, v) in vs.iter().enumerate() {
                if matches!(v, Value::Null) { nulls += 1 } else { bits[i / 8] |= 1 << (i % 8) }
            }
            let data: Vec<Vec<u8>> = match *k {
                "l" => vec![vs.iter().flat_map(|v| match v { Value::Int(n) => *n, _ => 0 }.to_le_bytes()).collect()],
                "g" => vec![vs.iter().flat_map(|v| match v { Value::Float(d) => *d, Value::Int(n) => *n as f64, _ => 0.0 }.to_le_bytes()).collect()],
                "b" => {
                    let mut b = vec![0u8; rows.div_ceil(8)];
                    for (i, v) in vs.iter().enumerate() { if let Value::Bool(true) = v { b[i / 8] |= 1 << (i % 8) } }
                    vec![b]
                }
                "u" => {
                    let mut offs: Vec<u8> = Vec::with_capacity((rows + 1) * 4);
                    let mut chars = Vec::new();
                    offs.extend_from_slice(&0i32.to_le_bytes());
                    for v in vs {
                        if let Value::Str(x) = v { chars.extend_from_slice(x.as_bytes()) }
                        offs.extend_from_slice(&(chars.len() as i32).to_le_bytes());
                    }
                    vec![offs, chars]
                }
                _ => vec![],
            };
            let mut ptrs: Vec<*const c_void> = Vec::new();
            if *k != "n" {
                ptrs.push(if nulls == 0 { std::ptr::null() } else { bits.as_ptr() as *const c_void });
                bytes.push(bits);
                for d in data {
                    ptrs.push(d.as_ptr() as *const c_void);
                    bytes.push(d);
                }
            }
            arrays.push(FFI_ArrowArray {
                length: rows as i64, null_count: if *k == "n" { rows as i64 } else { nulls }, offset: 0,
                n_buffers: ptrs.len() as i64, n_children: 0, buffers: ptrs.as_mut_ptr(), children: std::ptr::null_mut(),
                dictionary: std::ptr::null_mut(), release: Some(release_child_array), private_data: std::ptr::null_mut() });
            buffer_ptrs.push(ptrs);
        }
        strings.extend(child_formats);
        // taken once every child is in place: a Vec's heap buffer does not move with the Vec
        let mut schema_ptrs: Vec<*mut FFI_ArrowSchema> = schemas.iter_mut().map(|b| b as *mut _).collect();
        let mut array_ptrs: Vec<*mut FFI_ArrowArray> = arrays.iter_mut().map(|b| b as *mut _).collect();
        let mut top_buffers: Vec<*const c_void> = vec![std::ptr::null()];
        *array_out = FFI_ArrowArray {
            length: rows as i64, null_count: 0, offset: 0, n_buffers: 1, n_children: cols.len() as i64,
            buffers: top_buffers.as_mut_ptr(), children: array_ptrs.as_mut_ptr(), dictionary: std::ptr::null_mut(),
            release: Some(release_array), private_data: std::ptr::null_mut() };
        *schema_out = FFI_ArrowSchema {
            format: top.as_ptr(), name: std::ptr::null(), metadata: std::ptr::null(), flags: 0, n_children: cols.len() as i64,
            children: schema_ptrs.as_mut_ptr(), dictionary: std::ptr::null_mut(), release: Some(release_schema),
            private_data: std::ptr::null_mut() };
        buffer_ptrs.push(top_buffers);
        strings.push(top);
        // the schema side and the array side are released apart, so each owns its half
        (*schema_out).private_data = Box::into_raw(Box::new(Owned { _strings: strings, _bytes: Vec::new(), _schemas: schemas,
            _arrays: Vec::new(), _schema_ptrs: schema_ptrs, _array_ptrs: Vec::new(), _buffer_ptrs: Vec::new() })) as *mut c_void;
        (*array_out).private_data = Box::into_raw(Box::new(Owned { _strings: Vec::new(), _bytes: bytes, _schemas: Vec::new(),
            _arrays: arrays, _schema_ptrs: Vec::new(), _array_ptrs: array_ptrs, _buffer_ptrs: buffer_ptrs })) as *mut c_void;
        Ok(())
    }
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

        /// a TABLE call (foreign-arrow-ffm): the head in `req`, the table as
        /// Arrow C Data in, a table answer as Arrow C Data out
        #[no_mangle]
        pub extern "C" fn okay_exchange_table(req: *const u8, len: usize, out_len: *mut usize,
                                              schema_in: *mut $crate::cdata::FFI_ArrowSchema, array_in: *mut $crate::cdata::FFI_ArrowArray,
                                              schema_out: *mut $crate::cdata::FFI_ArrowSchema, array_out: *mut $crate::cdata::FFI_ArrowArray) -> *mut u8 {
            // SAFETY: the host hands `len` readable bytes at `req`, a writable `usize` at `out_len`, and four C Data structs
            let bytes = if req.is_null() || len == 0 { &[][..] } else { unsafe { std::slice::from_raw_parts(req, len) } };
            let mut out = unsafe { $crate::exchange_table_in(&OKAY_WORKER, $make, bytes, schema_in, array_in, schema_out, array_out) }.into_boxed_slice();
            unsafe { *out_len = out.len() };
            let p = out.as_mut_ptr();
            std::mem::forget(out);
            p
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
