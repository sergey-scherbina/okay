## s3-multipart-put — okay-blob's S3 puts in parts, in constant memory

`S3.put` pulls its source until it holds `partSize` bytes (8 MiB by
default, S3's 5 MiB floor enforced): a shorter body is one signed PUT
as before, a longer one an S3 multipart upload holding one part at a
time, each part signed with its real hash. A put that fails aborts its
upload, so nothing half-written is ever an object; `pending(key)` and
`abandon(key, id)` clean up after a process that died mid-put. Found
on the way: MinIO's `&#34;` in a completed upload's ETag (now
unescaped) and its exact-key-only upload listing. `TestS3Multipart`
(Live, MinIO); part of engine-object-store-io.
