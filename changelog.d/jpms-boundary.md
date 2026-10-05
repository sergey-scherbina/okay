## JPMS Boundary Evidence

Stage A adds descriptor names/requires, conservative per-rule enforcement,
split packages, launcher options and Audit.runtime() to okay-audit.
Text and JSON retain scanner findings alongside this evidence; unnamed
inputs remain scan-only. Manifest-relative jvmOptions records security
flags, including add-reads overrides. Stage B/C deployment remains in
backlog.d/okay-audit/jpms-deployment.md.

Implementation is the commit adding this entry. TestAudit covers directory
and JAR descriptors, readability, split deduplication, options and runtime.
Verification: 203 tests in okayAudit/okayDeploy, no compiler warnings;
root audit passed, dogfood/report.txt regenerated and JSON parsed.
The scoped affected gate names audit sources and docs explicitly: the sole
build change wires the existing okay-test facade into this suite's test
classpath, not any production module. Whole-build CI remains runner-owned.
