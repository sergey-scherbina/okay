## core-pin-bind-fused — okay-core pinned at 15d4386: a bind of a map one bind

okay-core fuses a bind of a map into one bind and an operation mapped twice into one `MapK` (the functions composed,
`Cont.Fused`): kernel capture −22%, facade capture −20%, captureShallow −11%, generate −5% (`-wm BULK`); okay-core
green on JVM, JS and Native (273, 270, 273). Here: the whole build compiled with no warnings (276 module compiles);
okay, okay-std, okay-async, okay-stream, okay-tx, okay-durable and okay-freer pass on the JVM, okay and okay-std on
JS, okay, okay-std and okay-stm on Native; okay-cats 328 of 329, the one its onCancel law (backlog
cats-js-oncancel-law). Suites named by hand.
