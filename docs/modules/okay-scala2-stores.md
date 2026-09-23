# okay-scala2-stores

okay-cache, okay-blob and okay-docs for **Scala 2.13**. Each store is built
with its own constructor (`Cache.memory(regime, max)`, `Fs(root)`,
`S3.wired(...)`), and its values (`Regime`, `Etag`, `Meta`, `Cond`,
`PutResult`) are used directly. Every operation is a program, and these
three objects provide them:

| | |
|---|---|
| `Caches` | `get`, `put`, `invalidate`, single-flight `getOrLoad`, `writeThrough`, cross-node `drain`, a `View`'s `latest` |
| `Blobs` | `put` a stream of chunks, `putBytes`, `putFile`, `getBytes`, `stream`, `head`, `list`, `delete`, `backup`/`restore` of a log |
| `Documents` | `onTopic`, conditional `put`/`delete`, `get` with its version, `query` by an indexed field |

The names differ from `Cache`, `Blob` and `Docs` so that both packages
can be imported with wildcards.

The walkthrough is section 8m of
[okay from Scala 2.13](../scala2.md#8m-stores-cache-blob-documents), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
