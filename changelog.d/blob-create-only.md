## blob-create-only — exclusive immutable S3 chunks and markers

Implemented in c59217c78: ConditionalObjects capability, bounded signed
If-None-Match PUT and bounded GET with response release on errors.
Existing Blob PUT behavior unchanged; no conditional-write fallback.
Two scoped tests and affected compile green, no warnings.
