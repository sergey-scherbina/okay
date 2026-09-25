## okay-arrow-file - the Arrow IPC FILE format, both implementations

okay-arrow stage 6 (specs/okay-arrow.md): `writeFile`, `readFile`,
`fileBatches` and `readFileBatch` on the facade. The file is pyarrow's
layout, with a footer, so any batch is found without reading the ones
before it. `OkayArrow` works on every platform, compressed too;
`ApacheArrow` goes through Arrow Java's file writer and reader. pyarrow's
`open_file` validates ours, and ours reads pyarrow's by index.

FIXED along the way (found by the stage's mutant): a read Arrow Java
failed partway through reported "Memory was leaked" instead of its own
error, because `ApacheArrow` closed its allocator in a `finally`. The first
failure is now kept, and the test for it was watched red first.
