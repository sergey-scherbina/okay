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
	"bytes"
	"compress/flate"
	"encoding/binary"
	"encoding/json"
	"io"
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
	case nil, bool, string, int64, float64:
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
			// a JSON text gives a json.Number; CBOR gives the number itself
			switch n := x["v"].(type) {
			case json.Number:
				f, _ := strconv.ParseFloat(string(n), 64)
				return f
			case int64:
				return float64(n)
			case float64:
				return n
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
// that calls okay's effects with okay.Call(c, request) and gets the answer
// (polyglot-one-wire: okay_call(request) -> answer, the one name in every
// language; Go spells it okay.Call, because an exported Go name begins with
// a capital and a goroutine has no local storage to hide c in). Each
// is answered once, so a handler that resumes twice (Choice) needs a
// Program instead.
type Functions map[string]func(c *Ctx, args []any) any

// OkayError is a callback that failed in okay: its condition's kind and message.
type OkayError struct{ Kind, Message string }

func (e *OkayError) Error() string { return e.Kind + ": " + e.Message }

// Ctx is a direct-style call in progress: what okay.Call reaches okay through.
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

// Call is okay_call(request) -> answer: it performs an okay operation from
// ordinary Go. The host runs its callback under the CALLER's handlers, and
// this returns the answer, typed by the generated operation
// (shop.PriceOf(sku)), or okay.Named(name, args...) untyped. An error when
// the callback failed in okay, or was not offered to this call.
func Call[A any](c *Ctx, request Op[A]) (A, error) {
	var zero A
	if !c.offered[request.Name] {
		return zero, &OkayError{"LookupError", fmt.Sprintf("okay.Call(%q): this call was offered %v", request.Name, keys(c.offered))}
	}
	*c.next++
	k := *c.next
	wire := make([]any, len(request.Args))
	for i, a := range request.Args {
		wire[i] = enc(a)
	}
	c.events <- event{ask: map[string]any{"ask": map[string]any{"cb": request.Name, "args": wire, "k": k}}}
	a := <-c.answers
	if a.err != nil {
		return zero, a.err
	}
	return request.Decode(a.value)
}

// Named is an untyped request: the operation name, its arguments, and the
// answer as the wire decoded it.
func Named(name string, args ...any) Op[any] {
	return Op[any]{Name: name, Args: args, Decode: func(v any) (any, error) { return v, nil }}
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
	format    string // "json" or "cbor" (stage 5a); json until a configure
	compress  string // "none" or "deflate"
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
	return &Worker{format: "json", compress: "none", programs: programs, functions: fs, konts: map[key]func(any) Prog{}, waiting: map[int64]*Ctx{}}
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

// Hello is the handshake line a worker speaks first. It announces the
// formats and compressions this worker can be configured to (stage 5a).
func Hello() string {
	b, _ := json.Marshal(map[string]any{"shim": ShimVersion, "python": "go",
		"speaks": map[string]any{"format": []any{"json", "cbor"}, "compress": []any{"deflate"}}})
	return string(b)
}

// Framed: after a configure other than the defaults, the wire carries
// frames (a 4-byte big-endian length, then the bytes), not lines.
func (w *Worker) Framed() bool { return w.format != "json" || w.compress != "none" }

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
	case "configure":
		f, _ := req["format"].(string)
		c, _ := req["compress"].(string)
		if f != "json" && f != "cbor" {
			return condition(id, "ValueError", fmt.Sprintf("this Go worker speaks the formats json, cbor; not %q", f))
		}
		if c != "none" && c != "deflate" {
			return condition(id, "ValueError", fmt.Sprintf("this Go worker speaks the compressions none, deflate; not %q", c))
		}
		return map[string]any{"id": id, "ok": map[string]any{"format": f, "compress": c}}
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

// HandleMessage answers one message, in the worker's current encoding, with
// one message in the same encoding. A configure takes effect AFTER its own
// answer, which goes out in the encoding it was asked in.
func (w *Worker) HandleMessage(msg []byte) []byte {
	var reply map[string]any
	raw, err := w.decode(msg)
	if err != nil {
		reply = condition(nil, "ValueError", err.Error())
	} else {
		req := map[string]any{}
		for k, v := range raw {
			req[k] = dec(v)
		}
		reply = w.answer(raw["id"], req)
	}
	out := w.encode(reply)
	if ok, isMap := reply["ok"].(map[string]any); isMap && reply["condition"] == nil {
		if f, has := ok["format"].(string); has {
			if c, has := ok["compress"].(string); has && raw["op"] == "configure" {
				w.format, w.compress = f, c
			}
		}
	}
	return out
}

// Handle answers one request line with one answer line (the JSON-lines wire).
func (w *Worker) Handle(line string) string { return string(w.HandleMessage([]byte(line))) }

func (w *Worker) decode(msg []byte) (map[string]any, error) {
	if w.compress == "deflate" {
		r := flate.NewReader(bytes.NewReader(msg))
		b, err := io.ReadAll(r)
		if err != nil {
			return nil, fmt.Errorf("a DEFLATE message did not inflate: %v", err)
		}
		msg = b
	}
	if w.format == "cbor" {
		v, rest, err := cborDecode(msg)
		if err != nil {
			return nil, err
		}
		if len(rest) != 0 {
			return nil, fmt.Errorf("CBOR: %d bytes after the message", len(rest))
		}
		m, ok := v.(map[string]any)
		if !ok {
			return nil, fmt.Errorf("CBOR: a request is a map")
		}
		return m, nil
	}
	d := json.NewDecoder(bytes.NewReader(msg))
	d.UseNumber()
	var raw map[string]any
	if err := d.Decode(&raw); err != nil {
		return nil, fmt.Errorf("not a JSON request")
	}
	return raw, nil
}

func (w *Worker) encode(reply map[string]any) []byte {
	var out []byte
	if w.format == "cbor" {
		out = cborEncode(nil, reply)
	} else {
		b, err := json.Marshal(reply)
		if err != nil {
			b, _ = json.Marshal(condition(reply["id"], "GoError", err.Error()))
		}
		out = b
	}
	if w.compress == "deflate" {
		var buf bytes.Buffer
		zw, _ := flate.NewWriter(&buf, flate.DefaultCompression)
		zw.Write(out)
		zw.Close()
		out = buf.Bytes()
	}
	return out
}

// ------------------------------------------------------------- CBOR (RFC 8949)

func cborHead(out []byte, major byte, n uint64) []byte {
	m := major << 5
	switch {
	case n < 24:
		return append(out, m|byte(n))
	case n < 1<<8:
		return append(out, m|24, byte(n))
	case n < 1<<16:
		return append(out, m|25, byte(n>>8), byte(n))
	case n < 1<<32:
		return append(out, m|26, byte(n>>24), byte(n>>16), byte(n>>8), byte(n))
	}
	return append(out, m|27, byte(n>>56), byte(n>>48), byte(n>>40), byte(n>>32), byte(n>>24), byte(n>>16), byte(n>>8), byte(n))
}

func cborEncode(out []byte, v any) []byte {
	switch x := v.(type) {
	case nil:
		return append(out, 0xf6)
	case bool:
		if x {
			return append(out, 0xf5)
		}
		return append(out, 0xf4)
	case int:
		return cborEncode(out, int64(x))
	case int64:
		if x >= 0 {
			return cborHead(out, 0, uint64(x))
		}
		return cborHead(out, 1, uint64(-1-x))
	case float64:
		out = append(out, 0xfb)
		return binary.BigEndian.AppendUint64(out, math.Float64bits(x))
	case json.Number:
		if i, err := x.Int64(); err == nil {
			return cborEncode(out, i)
		}
		f, _ := x.Float64()
		return cborEncode(out, f)
	case string:
		out = cborHead(out, 3, uint64(len(x)))
		return append(out, x...)
	case []any:
		out = cborHead(out, 4, uint64(len(x)))
		for _, e := range x {
			out = cborEncode(out, e)
		}
		return out
	case map[string]any:
		keys := make([]string, 0, len(x))
		for k := range x {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		out = cborHead(out, 5, uint64(len(keys)))
		for _, k := range keys {
			out = cborEncode(out, k)
			out = cborEncode(out, x[k])
		}
		return out
	}
	panic(fmt.Sprintf("a %T does not encode as CBOR", v))
}

func cborArg(info byte, b []byte) (uint64, []byte, error) {
	need := map[byte]int{24: 1, 25: 2, 26: 4, 27: 8}
	if info < 24 {
		return uint64(info), b, nil
	}
	n, ok := need[info]
	if !ok {
		return 0, nil, fmt.Errorf("CBOR: an indefinite or reserved length (%d) is not in the wire's subset", info)
	}
	if len(b) < n {
		return 0, nil, fmt.Errorf("a CBOR message ended early (cut short?)")
	}
	var v uint64
	for _, c := range b[:n] {
		v = v<<8 | uint64(c)
	}
	return v, b[n:], nil
}

func cborDecode(b []byte) (any, []byte, error) {
	if len(b) == 0 {
		return nil, nil, fmt.Errorf("a CBOR message ended early (cut short?)")
	}
	major, info, b := b[0]>>5, b[0]&0x1f, b[1:]
	if major == 7 {
		switch info {
		case 20:
			return false, b, nil
		case 21:
			return true, b, nil
		case 22, 23:
			return nil, b, nil
		case 25, 26, 27:
			bits, rest, err := cborArg(info, b)
			if err != nil {
				return nil, nil, err
			}
			switch info {
			case 25:
				return halfFloat(uint16(bits)), rest, nil
			case 26:
				return float64(math.Float32frombits(uint32(bits))), rest, nil
			}
			return math.Float64frombits(bits), rest, nil
		}
		return nil, nil, fmt.Errorf("CBOR: simple value %d is not in the wire's subset", info)
	}
	n, b, err := cborArg(info, b)
	if err != nil {
		return nil, nil, err
	}
	switch major {
	case 0:
		return int64(n), b, nil
	case 1:
		return -1 - int64(n), b, nil
	case 3:
		if uint64(len(b)) < n {
			return nil, nil, fmt.Errorf("a CBOR string ended early (cut short?)")
		}
		return string(b[:n]), b[n:], nil
	case 4:
		xs := make([]any, 0, n)
		for i := uint64(0); i < n; i++ {
			var x any
			x, b, err = cborDecode(b)
			if err != nil {
				return nil, nil, err
			}
			xs = append(xs, x)
		}
		return xs, b, nil
	case 5:
		m := map[string]any{}
		for i := uint64(0); i < n; i++ {
			var k, v any
			k, b, err = cborDecode(b)
			if err != nil {
				return nil, nil, err
			}
			ks, ok := k.(string)
			if !ok {
				return nil, nil, fmt.Errorf("CBOR: a map key that is not text")
			}
			v, b, err = cborDecode(b)
			if err != nil {
				return nil, nil, err
			}
			m[ks] = v
		}
		return m, b, nil
	}
	return nil, nil, fmt.Errorf("CBOR: major type %d (byte strings, tags) is not in the wire's subset", major)
}

func halfFloat(h uint16) float64 {
	exp := int(h>>10) & 0x1f
	mant := float64(h & 0x3ff)
	var v float64
	switch exp {
	case 0:
		v = mant * math.Pow(2, -24)
	case 31:
		if mant == 0 {
			v = math.Inf(1)
		} else {
			v = math.NaN()
		}
	default:
		v = (mant + 1024) * math.Pow(2, float64(exp-25))
	}
	if h&0x8000 != 0 {
		return -v
	}
	return v
}

// serveLines speaks the wire over one stream pair: JSON lines until a
// configure asks for more, frames after it (stage 5a).
func serveLines(w *Worker, in *bufio.Reader, out *bufio.Writer) {
	out.WriteString(Hello())
	out.WriteByte('\n')
	out.Flush()
	for {
		if w.Framed() {
			var n [4]byte
			if _, err := io.ReadFull(in, n[:]); err != nil {
				return
			}
			msg := make([]byte, binary.BigEndian.Uint32(n[:]))
			if _, err := io.ReadFull(in, msg); err != nil {
				return
			}
			reply := w.HandleMessage(msg)
			var m [4]byte
			binary.BigEndian.PutUint32(m[:], uint32(len(reply)))
			out.Write(m[:])
			out.Write(reply)
			out.Flush()
			continue
		}
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

// ---------------------------------------------------------------- in-process

var exported *Worker

// Export makes programs and functions the worker a WebAssembly build serves
// IN-PROCESS (okay_exchange, in okay_wasm.go). Call it from init(): a
// reactor module's main never runs.
func Export(programs Programs, functions ...Functions) {
	exported = NewWorker(programs, functions...)
}

// Exchange is one in-process exchange: an empty request answers the handshake.
func Exchange(msg []byte) []byte {
	if len(bytes.TrimSpace(msg)) == 0 {
		return []byte(Hello())
	}
	if exported == nil {
		return []byte(`{"id":null,"condition":{"kind":"LookupError","message":"okay.Export was never called: nothing is served"}}`)
	}
	return exported.HandleMessage(msg)
}
