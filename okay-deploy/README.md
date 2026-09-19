# okay-deploy

A deployment is a VALUE (specs/deployment.md). An application declares a `Deployment(...)` in its own module — its services and what each of them needs — and okay-deploy renders that value into the wires operators already run: a Dockerfile, a compose file, systemd units, a Helm chart, fly/render/railway manifests, Terraform for AWS. A one-line test keeps the committed files equal to the value, and okay-deploy itself knows no application.

**Depends on:** `okay-codec` (a `Deploy` has a Schema). Build half:

This page is a pointer, not a guide: the module's own doc
below carries the pieces, the decisions and the measurements.

## Further

| | |
|---|---|
| [`docs/modules/okay-deploy.md`](../docs/modules/okay-deploy.md) | what it is, and the reasoning |
| [`specs/deployment.md`](../specs/deployment.md) | the design and its decisions |
