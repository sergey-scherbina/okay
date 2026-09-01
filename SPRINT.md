# Sprint

## Doing
- blob-s3 — stage 1: the S3 REST subset with OWN SigV4 over the
  okay-http client, the AWS doc vector pinning the signing; puts
  spool to disk so the payload hash is real and memory constant;
  BlobContract re-runs against MinIO, skipping where absent
  (specs/blob.md)

## Queue
(other candidates from BACKLOG.md: the roads the landed Sql seam
 unblocks — sql-pg-wire, lake-read-duckdb, jdbc-write-bridge,
 rag-pgvector — plus persist-wire and cache-memory; ui-durable and
 mcp-resumable-sse can bind to persist stage 1's tail/offsets)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
