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

// Serve is the worker's main loop: named programs, served on stdin/stdout.
func Serve(programs map[string]func(args []any) Prog) {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	konts := map[key]func(any) Prog{}
	var next int64
	say := func(v map[string]any) {
		b, err := json.Marshal(v)
		if err != nil {
			b, _ = json.Marshal(map[string]any{"id": v["id"], "condition": map[string]any{"kind": "GoError", "message": err.Error()}})
		}
		out.Write(b)
		out.WriteByte('\n')
		out.Flush()
	}
	say(map[string]any{"shim": ShimVersion, "python": "go"})
	node := func(run int64, p Prog) map[string]any {
		if p.done {
			return map[string]any{"done": enc(p.value)}
		}
		next++
		konts[key{run, next}] = p.k
		args := make([]any, len(p.args))
		for i, a := range p.args {
			args[i] = enc(a)
		}
		return map[string]any{"perform": p.name, "args": args, "k": next}
	}
	condition := func(id any, kind, msg string) map[string]any {
		return map[string]any{"id": id, "condition": map[string]any{"kind": kind, "message": msg}}
	}
	answer := func(id any, req map[string]any) (reply map[string]any) {
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
			f, ok := programs[fn]
			if !ok {
				return condition(id, "LookupError", fmt.Sprintf("no program named '%s' in this worker", fn))
			}
			args, _ := req["args"].([]any)
			return map[string]any{"id": id, "ok": node(run, f(args))}
		case "continue":
			k, _ := req["k"].(int64)
			f, ok := konts[key{run, k}]
			if !ok {
				return condition(id, "LookupError", fmt.Sprintf("continuation %d of run %d is not held here (forgotten, or another process)", k, run))
			}
			return map[string]any{"id": id, "ok": node(run, f(req["answer"]))}
		case "forget":
			for kk := range konts {
				if kk.run == run {
					delete(konts, kk)
				}
			}
			return map[string]any{"id": id, "ok": nil}
		}
		return condition(id, "ValueError", fmt.Sprintf("this Go worker serves programs only, not '%v'", req["op"]))
	}
	for {
		line, err := in.ReadString('\n')
		if len(strings.TrimSpace(line)) > 0 {
			d := json.NewDecoder(strings.NewReader(line))
			d.UseNumber()
			var raw map[string]any
			if e := d.Decode(&raw); e != nil {
				say(condition(nil, "ValueError", "not a JSON request"))
			} else {
				req := map[string]any{}
				for k, v := range raw {
					req[k] = dec(v)
				}
				// the wire's ids and run numbers are plain JSON integers
				say(answer(raw["id"], req))
			}
		}
		if err != nil {
			return
		}
	}
}
