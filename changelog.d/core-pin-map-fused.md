## core-pin-map-fused — okay-core pinned at 645c2e0: a map of a map one map

okay-core's `Cont.map` fuses a map of a map into one (`Cont.Fused`, bounded at 32 functions): a chain of maps is one
frame and one value at the run. Kernel capture −18%, captureDeep −37%, facade capture −17% (`-wm BULK`); okay-core
green on JVM, JS and Native (271, 268, 271). Here: the whole build compiled with no warnings (276 module compiles);
okay, okay-std, okay-async, okay-stream, okay-tx, okay-durable and okay-freer pass on the JVM, okay and okay-std on
JS, okay, okay-std and okay-stm on Native; okay-cats 328 of 329, the one its onCancel law (backlog
cats-js-oncancel-law). Suites named by hand: `affected` sees no change for a pin.
