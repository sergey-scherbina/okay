- [ ] polyglot-go — possible, and of the three the LEAST worth doing
      in-process. `go build -buildmode=c-shared` exports a C ABI, but it
      loads a whole second runtime into the JVM: its own GC, its own
      scheduler, its own signal handlers (Go installs SA_ONSTACK
      handlers the JVM also wants), and a cgo call from a foreign thread
      costs a thread switch. One Go runtime per process, fixed. Go code
      is overwhelmingly SERVICES, and a service's seam is the network,
      which okay-http and a future gRPC client already speak. So: (1) a
      subprocess handler on polyglot-remote-foreign only if a consumer
      needs Go code to perform okay effects (not otherwise); (2) a Go
      plugin compiled to Wasm (TinyGo) through polyglot-rust's Chicory
      road. In-process c-shared: refused, with the reasons above.
