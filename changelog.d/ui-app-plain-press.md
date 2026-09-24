## ui-app-plain-press — a plain mount is sent in the background too (2026-09-24)

Follow-up of ui-app-shell: `Enhance` skipped every `form.okay-plain`,
taking it for the live road's, so a page built on a PLAIN mount
(`Html.form`, no live client) reloaded and jumped on every press. It now
skips such a form only while the live client is on the page
(`window.okayLive`). specs/ui-app.md; TestAppShell holds the hook.
