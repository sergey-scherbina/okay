//go:build wasip1

// The okay wire IN-PROCESS for a Go module compiled to WebAssembly
// (GOOS=wasip1 GOARCH=wasm -buildmode=c-shared; polyglot-one-wire stage 3):
// okay_exchange(req, len, out_len) -> resp, and the buffers a host needs to
// reach this module's memory. The host is Chicory, inside the JVM.
package okay

import "unsafe"

// every buffer handed out, kept alive until okay_free
var held = map[uintptr][]byte{}

func keep(b []byte) int32 {
	if len(b) == 0 {
		b = make([]byte, 1)
	}
	p := uintptr(unsafe.Pointer(&b[0]))
	held[p] = b
	return int32(p)
}

//go:wasmexport okay_alloc
func okayAlloc(n int32) int32 {
	if n < 1 {
		n = 1
	}
	return keep(make([]byte, n))
}

//go:wasmexport okay_free
func okayFree(p int32, _ int32) {
	delete(held, uintptr(p))
}

//go:wasmexport okay_exchange
func okayExchange(req int32, n int32, outLen int32) int32 {
	var msg []byte
	if n > 0 {
		msg = append([]byte(nil), unsafe.Slice((*byte)(unsafe.Pointer(uintptr(req))), n)...)
	}
	out := Exchange(msg)
	*(*uint32)(unsafe.Pointer(uintptr(outLen))) = uint32(len(out))
	return keep(out)
}
