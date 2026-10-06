## blob-create-only — exclusive immutable S3 chunks and markers

Implemented by the blob-create-only lane: ConditionalObjects capability, bounded signed
If-None-Match PUT and bounded GET with response release on errors.
Existing Blob PUT behavior unchanged; no conditional-write fallback.
Two scoped tests and affected compile green, no warnings.
