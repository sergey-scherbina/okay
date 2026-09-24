// A Go plugin for okay, compiled to WebAssembly (polyglot-go stage 2):
// SHA-256 from Go's standard library, over the same C-shaped ABI as the
// Rust kernel — pointers and lengths in the module's own memory.
package main

import (
	"crypto/sha256"
	"unsafe"
)

// keeps every buffer okay_alloc hands out alive until okay_free
var held = map[uintptr][]byte{}

//go:wasmexport okay_alloc
func okayAlloc(n int32) int32 {
	if n < 1 {
		n = 1
	}
	b := make([]byte, n)
	p := uintptr(unsafe.Pointer(&b[0]))
	held[p] = b
	return int32(p)
}

//go:wasmexport okay_free
func okayFree(p int32, _ int32) {
	delete(held, uintptr(p))
}

// okay_sha256 writes the 32-byte digest of in[0:n] to out; 0 when done
//
//go:wasmexport okay_sha256
func okaySha256(in int32, n int32, out int32) int32 {
	data := unsafe.Slice((*byte)(unsafe.Pointer(uintptr(in))), n)
	sum := sha256.Sum256(data)
	copy(unsafe.Slice((*byte)(unsafe.Pointer(uintptr(out))), 32), sum[:])
	return 0
}

// okay_panic panics: what a host sees when a plugin fails
//
//go:wasmexport okay_panic
func okayPanic() int32 {
	panic("the go plugin says no")
}

func main() {}
