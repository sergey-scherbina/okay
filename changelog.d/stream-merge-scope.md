## the machine's merge cancels what it no longer needs; its Flow is opaque

Lane stream-merge-scope. `StreamCont.merge` holds its pending pulls in a cancel
scope: a consumer that stops early (`take`) leaves no pull running. `Src.onDone`
runs an effect when a stream ends. `okay.streams.machine.Flow` is opaque, as the
classic's, so its `Streaming` instance is found without an import (`fromSrc` /
`toSrc` convert).
