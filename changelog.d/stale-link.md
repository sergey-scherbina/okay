## a link that fails on stale build output is named and cleaned, not filed as noise

Lane stale-link. A test or source that moved to another module or package
(classic-to-freer, okay-std) leaves its old .nir/.sjsir in target/, and the
next Native or JS link fails on classes no source has any more — two whole
builds on master were filed as "infrastructure noise" for it (2026-10-07).
`gate.sh` now names a failed link and its modules (`gate: link-clean: …`);
`ci-runner.sh` cleans those modules and gates the whole build once more, and
a link that fails again after the clean is reported as a real error.
