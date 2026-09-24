// Package okay is okay's programs-as-data for Go (polyglot-go,
// specs/polyglot-go.md). A Go program is written in Prog: Done answers,
// Perform asks okay to run a named operation (a callback the Scala side
// offered) and continues with its answer. Serve speaks the okay wire on
// stdin/stdout, so okay-py's engine drives a Go worker exactly as it drives
// Python and Haskell.
//
// A continuation is an ordinary Go closure. The worker keeps it under an id
// until okay forgets the run, so okay may continue it more than once: a
// Choice handler on the Scala side makes every branch.
//
// Standard library only.
package okay

import (
	"bufio"
	"encoding/json"
	"fmt"
	"math"
	"math/big"
	"net"
	"os"
	"sort"
	"strconv"
	"strings"
)

// ShimVersion is the wire version this worker speaks; the host refuses any other.
const ShimVersion = 6

// KV is one entry of a Dict.
type KV struct {
	Key string
	Val any
}

// Dict is a mapping that keeps its order, as the wire's dicts do.
type Dict []KV

// Prog is a program as data: an answer, or a named operation and the
// function that continues with its answer.
type Prog struct {
	done  bool
	value any
	name  string
	args  []any
	k     func(any) Prog
}

// Done is a program that has answered v.
func Done(v any) Prog { return Prog{done: true, value: v} }

// Perform asks okay to run the operation name; the program continues with its answer.
func Perform(name string, args ...any) Prog {
	return Prog{name: name, args: args, k: Done}
}

// Then is p, then f of its answer.
func (p Prog) Then(f func(any) Prog) Prog {
	if p.done {
		return f(p.value)
	}
	k := p.k
	return Prog{name: p.name, args: p.args, k: func(x any) Prog { return k(x).Then(f) }}
}

// ------------------------------------------------------------ typed operations

// Op is one operation of a program's effects, typed by its answer: its name,
// its arguments, and how its answer is read. Go.ops in Scala writes one
// constructor per operation from the callbacks' Schemas.
type Op[A any] struct {
	Name   string
	Args   []any
	Decode func(any) (A, error)
}

// Program is a program answering A.
type Program[A any] struct{ p Prog }

// Pure is a program that has answered a.
func Pure[A any](a A) Program[A] { return Program[A]{Done(a)} }

// Send performs op; the program's answer is op's typed answer. A value
// that does not decode is a panic, which the worker reports as a condition.
func Send[A any](op Op[A]) Program[A] {
	return Program[A]{Perform(op.Name, op.Args...).Then(func(v any) Prog {
		a, err := op.Decode(v)
		if err != nil {
			panic(fmt.Sprintf("%s answered %v: %v", op.Name, v, err))
		}
		return Done(a)
	})}
}

// Bind is m, then f of its answer.
func Bind[A, B any](m Program[A], f func(A) Program[B]) Program[B] {
	return Program[B]{m.p.Then(func(v any) Prog { return f(v.(A)).p })}
}

// Untyped is the program for Serve.
func (m Program[A]) Untyped() Prog { return m.p }

// Int reads an integer answer.
func Int(v any) (int64, error) {
	switch x := v.(type) {
	case int64:
		return x, nil
	case float64:
		if x == math.Trunc(x) {
			return int64(x), nil
		}
	}
	return 0, fmt.Errorf("not an integer")
}

// Float reads a number.
func Float(v any) (float64, error) {
	switch x := v.(type) {
	case float64:
		return x, nil
	case int64:
		return float64(x), nil
	}
	return 0, fmt.Errorf("not a number")
}

// String reads a string.
func String(v any) (string, error) {
	if s, ok := v.(string); ok {
		return s, nil
	}
	return "", fmt.Errorf("not a string")
}

// Bool reads a boolean.
func Bool(v any) (bool, error) {
	if b, ok := v.(bool); ok {
		return b, nil
	}
	return false, fmt.Errorf("not a boolean")
}

// Any reads an answer as the wire's value, undecoded: an operation whose
// Scala type Go has no name for.
func Any(v any) (any, error) { return v, nil }

// List is a typed slice as the wire's list, for an operation's argument.
func List[A any](xs []A) []any {
	out := make([]any, len(xs))
	for i, x := range xs {
		out[i] = x
	}
	return out
}

// ListOf reads a list, each element by item.
func ListOf[A any](item func(any) (A, error)) func(any) ([]A, error) {
	return func(v any) ([]A, error) {
		xs, ok := v.([]any)
		if !ok {
			return nil, fmt.Errorf("not a list")
		}
		out := make([]A, len(xs))
		for i, x := range xs {
			a, err := item(x)
			if err != nil {
				return nil, fmt.Errorf("element %d: %v", i, err)
			}
			out[i] = a
		}
		return out, nil
	}
}

// --------------------------------------------------------- the wire's values

const exact = int64(1) << 53

func enc(v any) any {
	switch x := v.(type) {
	case nil:
		return nil
	case bool, string:
		return x
	case int:
		return enc(int64(x))
	case int64:
		if x > -exact && x < exact {
			return x
		}
		return map[string]any{"t": "int", "v": strconv.FormatInt(x, 10)}
	case *big.Int:
		return map[string]any{"t": "int", "v": x.String()}
	case float64:
		if math.IsNaN(x) {
			return map[string]any{"t": "nan"}
		}
		if x == math.Trunc(x) && math.Abs(x) < 1e15 {
			return map[string]any{"t": "f", "v": x}
		}
		return x
	case []any:
		out := make([]any, len(x))
		for i, e := range x {
			out[i] = enc(e)
		}
		return out
	case []string:
		out := make([]any, len(x))
		for i, e := range x {
			out[i] = e
		}
		return out
	case Dict:
		kv := make([]any, len(x))
		for i, e := range x {
			kv[i] = []any{e.Key, enc(e.Val)}
		}
		return map[string]any{"t": "dict", "kv": kv}
	case map[string]any:
		keys := make([]string, 0, len(x))
		for k := range x {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		d := make(Dict, len(keys))
		for i, k := range keys {
			d[i] = KV{k, x[k]}
		}
		return enc(d)
	}
	panic(fmt.Sprintf("a %T does not cross the okay wire", v))
}

func dec(j any) any {
	switch x := j.(type) {
	case nil, bool, string:
		return x
	case json.Number:
		s := string(x)
		if !strings.ContainsAny(s, ".eE") {
			if n, err := strconv.ParseInt(s, 10, 64); err == nil {
				return n
			}
			b, _ := new(big.Int).SetString(s, 10)
			return b
		}
		f, _ := strconv.ParseFloat(s, 64)
		return f
	case []any:
		out := make([]any, len(x))
		for i, e := range x {
			out[i] = dec(e)
		}
		return out
	case map[string]any:
		switch x["t"] {
		case "nan":
			return math.NaN()
		case "f":
			if n, ok := x["v"].(json.Number); ok {
				f, _ := strconv.ParseFloat(string(n), 64)
				return f
			}
		case "int":
			if s, ok := x["v"].(string); ok {
				if n, err := strconv.ParseInt(s, 10, 64); err == nil {
					return n
				}
				b, _ := new(big.Int).SetString(s, 10)
				return b
			}
		case "dict":
			if kv, ok := x["kv"].([]any); ok {
				d := Dict{}
				for _, p := range kv {
					if pair, ok := p.([]any); ok && len(pair) == 2 {
						if k, ok := pair[0].(string); ok {
							d = append(d, KV{k, dec(pair[1])})
						}
					}
				}
				return d
			}
		}
		keys := make([]string, 0, len(x))
		for k := range x {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		d := make(Dict, len(keys))
		for i, k := range keys {
			d[i] = KV{k, dec(x[k])}
		}
		return d
	}
	return j
}

// ------------------------------------------------------------------ the worker

type key struct{ run, k int64 }

// Programs are programs as data, served by name: what multi-shot needs.
type Programs map[string]func(args []any) Prog

// Functions are DIRECT-STYLE functions, served by name: ordinary Go code
// that calls okay's effects with c.Call(name, args...) and gets the answer
// (polyglot-one-wire, the operator's okay_call(request) -> answer). Each
// is answered once, so a handler that resumes twice (Choice) needs a
// Program instead.
type Functions map[string]func(c *Ctx, args []any) any

// OkayError is a callback that failed in okay: its condition's kind and message.
type OkayError struct{ Kind, Message string }

func (e *OkayError) Error() string { return e.Kind + ": " + e.Message }

// Ctx is a direct-style call in progress: what c.Call reaches okay through.
type Ctx struct {
	id      any
	offered map[string]bool
	events  chan event
	answers chan answerMsg
	next    *int64
}

type event struct {
	ask   map[string]any // an ask for the host, or
	done  any            // the function's answer, or
	fault *OkayError     // its failure
}

type answerMsg struct {
	value any
	err   *OkayError
}

// Call performs the okay callback name with args: the host runs it under the
// caller's handlers, and this returns its answer. An error when the callback
// failed in okay, or was not offered to this call.
func (c *Ctx) Call(name string, args ...any) (any, error) {
	if !c.offered[name] {
		return nil, &OkayError{"LookupError", fmt.Sprintf("okay.Call(%q): this call was offered %v", name, keys(c.offered))}
	}
	*c.next++
	k := *c.next
	wire := make([]any, len(args))
	for i, a := range args {
		wire[i] = enc(a)
	}
	c.events <- event{ask: map[string]any{"ask": map[string]any{"cb": name, "args": wire, "k": k}}}
	a := <-c.answers
	if a.err != nil {
		return nil, a.err
	}
	return a.value, nil
}

// CallOp is Call, typed by a generated operation (Go.ops in Scala).
func CallOp[A any](c *Ctx, op Op[A]) (A, error) {
	var zero A
	v, err := c.Call(op.Name, op.Args...)
	if err != nil {
		return zero, err
	}
	return op.Decode(v)
}

func keys(m map[string]bool) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// Worker is the okay wire's protocol with no I/O (polyglot-one-wire): the
// programs it serves and the continuations it holds. Serve (a child
// process's pipes), ServeTCP (a socket) and an in-process export all use
// one, so a program behaves the same over every transport.
type Worker struct {
	programs  Programs
	functions Functions
	konts     map[key]func(any) Prog
	next      int64
	waiting   map[int64]*Ctx // direct-style calls parked in an ask, by k
	asks      int64
}

// NewWorker serves programs and, optionally, direct-style functions.
func NewWorker(programs Programs, functions ...Functions) *Worker {
	fs := Functions{}
	for _, f := range functions {
		for k, v := range f {
			fs[k] = v
		}
	}
	return &Worker{programs: programs, functions: fs, konts: map[key]func(any) Prog{}, waiting: map[int64]*Ctx{}}
}

// await is the next thing a direct-style call does: ask the host, or finish.
func (w *Worker) await(c *Ctx) map[string]any {
	e := <-c.events
	switch {
	case e.ask != nil:
		k := e.ask["ask"].(map[string]any)["k"].(int64)
		w.waiting[k] = c
		return e.ask
	case e.fault != nil:
		return condition(c.id, e.fault.Kind, e.fault.Message)
	}
	return map[string]any{"id": c.id, "ok": enc(e.done)}
}

// Hello is the handshake line a worker speaks first.
func Hello() string {
	b, _ := json.Marshal(map[string]any{"shim": ShimVersion, "python": "go"})
	return string(b)
}

func (w *Worker) node(run int64, p Prog) map[string]any {
	if p.done {
		return map[string]any{"done": enc(p.value)}
	}
	w.next++
	w.konts[key{run, w.next}] = p.k
	args := make([]any, len(p.args))
	for i, a := range p.args {
		args[i] = enc(a)
	}
	return map[string]any{"perform": p.name, "args": args, "k": w.next}
}

func condition(id any, kind, msg string) map[string]any {
	return map[string]any{"id": id, "condition": map[string]any{"kind": kind, "message": msg}}
}

func (w *Worker) answer(id any, req map[string]any) (reply map[string]any) {
	// a panic in the program (a failed type assertion, an index out of
	// range, an undecodable answer) is a CONDITION, and the worker lives on
	defer func() {
		if r := recover(); r != nil {
			reply = condition(id, "GoError", fmt.Sprint(r))
		}
	}()
	run, _ := req["run"].(int64)
	switch req["op"] {
	case "program":
		fn, _ := req["fn"].(string)
		f, ok := w.programs[fn]
		if !ok {
			return condition(id, "LookupError", fmt.Sprintf("no program named '%s' in this worker", fn))
		}
		args, _ := req["args"].([]any)
		return map[string]any{"id": id, "ok": w.node(run, f(args))}
	case "continue":
		k, _ := req["k"].(int64)
		f, ok := w.konts[key{run, k}]
		if !ok {
			return condition(id, "LookupError", fmt.Sprintf("continuation %d of run %d is not held here (forgotten, or another process)", k, run))
		}
		return map[string]any{"id": id, "ok": w.node(run, f(req["answer"]))}
	case "start":
		fn, _ := req["fn"].(string)
		f, ok := w.functions[fn]
		if !ok {
			return condition(id, "LookupError", fmt.Sprintf("no function named '%s' in this worker", fn))
		}
		args, _ := req["args"].([]any)
		offered := map[string]bool{}
		if cbs, ok := req["callbacks"].([]any); ok {
			for _, n := range cbs {
				if s, ok := n.(string); ok {
					offered[s] = true
				}
			}
		}
		c := &Ctx{id: id, offered: offered, events: make(chan event), answers: make(chan answerMsg), next: &w.asks}
		go func() {
			defer func() {
				if r := recover(); r != nil {
					if e, ok := r.(*OkayError); ok {
						c.events <- event{fault: e}
					} else {
						c.events <- event{fault: &OkayError{"GoError", fmt.Sprint(r)}}
					}
				}
			}()
			out := f(c, args)
			c.events <- event{done: out}
		}()
		return w.await(c)
	case "resume":
		k, _ := req["k"].(int64)
		c, ok := w.waiting[k]
		if !ok {
			return condition(id, "ValueError", fmt.Sprintf("resume %d: no call is waiting for it (resumed twice?)", k))
		}
		delete(w.waiting, k)
		if cond, ok := req["condition"].(Dict); ok {
			e := &OkayError{}
			for _, kv := range cond {
				if s, ok := kv.Val.(string); ok {
					if kv.Key == "kind" {
						e.Kind = s
					} else if kv.Key == "message" {
						e.Message = s
					}
				}
			}
			c.answers <- answerMsg{err: e}
		} else {
			c.answers <- answerMsg{value: req["ok"]}
		}
		return w.await(c)
	case "forget":
		for kk := range w.konts {
			if kk.run == run {
				delete(w.konts, kk)
			}
		}
		return map[string]any{"id": id, "ok": nil}
	}
	return condition(id, "ValueError", fmt.Sprintf("this Go worker serves programs only, not '%v'", req["op"]))
}

// Handle answers one request line with one answer line.
func (w *Worker) Handle(line string) string {
	d := json.NewDecoder(strings.NewReader(line))
	d.UseNumber()
	var raw map[string]any
	var reply map[string]any
	if e := d.Decode(&raw); e != nil {
		reply = condition(nil, "ValueError", "not a JSON request")
	} else {
		req := map[string]any{}
		for k, v := range raw {
			req[k] = dec(v)
		}
		reply = w.answer(raw["id"], req)
	}
	b, err := json.Marshal(reply)
	if err != nil {
		b, _ = json.Marshal(condition(reply["id"], "GoError", err.Error()))
	}
	return string(b)
}

// serveLines speaks the wire over one line-oriented stream pair.
func serveLines(w *Worker, in *bufio.Reader, out *bufio.Writer) {
	out.WriteString(Hello())
	out.WriteByte('\n')
	out.Flush()
	for {
		line, err := in.ReadString('\n')
		if len(strings.TrimSpace(line)) > 0 {
			out.WriteString(w.Handle(line))
			out.WriteByte('\n')
			out.Flush()
		}
		if err != nil {
			return
		}
	}
}

// Serve is the worker's main loop on stdin/stdout: a child process.
func Serve(programs Programs, functions ...Functions) {
	serveLines(NewWorker(programs, functions...), bufio.NewReader(os.Stdin), bufio.NewWriter(os.Stdout))
}

// ServeTCP serves the wire on a socket: another process, another machine.
// Each connection gets a Worker of its own, so one caller's continuations
// are never another's. Once bound it prints {"listening": "host:port"} on
// stdout, so a caller that asked for port 0 learns the port. PLAIN TCP,
// unauthenticated: a trusted network, or TLS or SSH in front of it.
func ServeTCP(addr string, programs Programs, functions ...Functions) error {
	l, err := net.Listen("tcp", addr)
	if err != nil {
		return err
	}
	b, _ := json.Marshal(map[string]any{"listening": l.Addr().String()})
	fmt.Println(string(b))
	for {
		c, err := l.Accept()
		if err != nil {
			return err
		}
		go func(c net.Conn) {
			defer c.Close()
			serveLines(NewWorker(programs, functions...), bufio.NewReader(c), bufio.NewWriter(c))
		}(c)
	}
}

// Main serves on TCP when OKAY_LISTEN names an address, on stdin/stdout
// otherwise: one binary, either transport.
func Main(programs Programs, functions ...Functions) {
	if addr := os.Getenv("OKAY_LISTEN"); addr != "" {
		if err := ServeTCP(addr, programs, functions...); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		return
	}
	Serve(programs, functions...)
}
