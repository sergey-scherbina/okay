## JPMS Boundary Evidence

Stage A adds descriptor names/requires, conservative per-rule enforcement,
split packages, launcher options and Audit.runtime() to okay-audit.
Text and JSON retain scanner findings alongside this evidence; unnamed
inputs remain scan-only. Manifest-relative jvmOptions records security
flags, including add-reads overrides. Stage B/C deployment remains in
backlog.d/okay-audit/jpms-deployment.md.

Implementation is the commit adding this entry. TestAudit covers directory
and JAR descriptors, readability, split deduplication, options and runtime.
