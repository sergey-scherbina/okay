- [x] optics-outside-conf — CLOSED 2026-09-11, with one test and no
      new abstraction. The PATH half is refused by a decision already
      in the tree: `Serve.Config` is "flat and scalar on purpose" and
      `Conf.fromEnv` refuses a nested field by name, so a lens into
      `server.tls.port` would be machinery for a shape nothing here
      has. The REFERENCE-TABLE half was already built — `envName` is
      one derivation for the reader and the renderer — and was
      under-consumed: the list was held against the deployment and
      against nothing a person opens, so `OKAY_ACME_EAB` was
      declared, deployable, read at boot and named in no guide.
      `TestScriptConfig` now asks that question of
      docs/okay-script-guide.md. The law runs one way only: a program
      reads variables its config does not declare (`OKAY_CONF` names
      the config FILE; `OKAY_STAGING` is okay-staging's switch), so
      "every OKAY_ in the guide is a setting" is false.
