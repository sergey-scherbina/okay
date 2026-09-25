## okay-arrow-stage0 - measured against Arrow Java: not faster, and why

The operator asked whether our Arrow is faster than Apache Arrow. It was
measured before any module code (`ArrowIpcBench`, Arrow Java 19.0.0,
500 000 rows, both sides reading the same stream, interop checked both
ways by `TestArrowJavaInterop`):

- through plain JVM arrays the two are level (write ~16 ms each);
- in each side's own columns Arrow Java is 5-30x ahead: its read is
  1-1.7 ms with 14 KB allocated, ours 30-42 ms with 136 MB — a `String`
  per row, where Arrow keeps offsets and bytes.

So okay-arrow's columns will be views over the message's bytes (read =
validate and slice, no copy), not arrays of values; `ArrowIpc`'s arrays
stay as the edge conversion. The new module holds the bench and the
interop test; Arrow Java is a test-only dependency. specs/okay-arrow.md.
