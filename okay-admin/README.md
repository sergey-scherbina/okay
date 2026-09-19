# okay-admin

Protected admin routes (specs/admin.md): a named action that DECLARES what it requires, and a table that refuses accordingly — the same bearer-token 401/403 ladder, now visible to a document. Extracted 2026-09-02 from `okay-demo`, fixing a real gap found while extracting it — `POST /admin/replay` had shipped with no authentication at all.

**Depends on:** `okay-security` (`Secure`, `Jwt`, `Verified`),

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-admin.md`](../docs/modules/okay-admin.md) | what it is, and the reasoning |
| [`specs/admin.md`](../specs/admin.md) | the design and its decisions |
