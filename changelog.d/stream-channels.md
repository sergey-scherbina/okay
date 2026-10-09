## channels on the machine; buffer in the stream front

Lane stream-channels. `StreamCont.send`/`receive` are a `Channel`'s send and
receive as operations of the machine; `fromChannel` reads a channel a chunk at a
time, `toChannel` pumps a stream into one, and `buffer(capacity)` reads a stream
through a channel filled by a producer fiber — a consumer that stops early
cancels the pump. `buffer` is a word of `Streaming` with both backends (the
classic's built from `drained` and a pump fiber, so it needs no `CanBlock`).
