## scoped-laws-forwarding-ref - the scoped-effects laws cite the forwarding law they lean on

Docs-only. specs/scoped-effects-laws.md said "forwarded UNCHANGED" and
"forwarded transparently" of `Effects.handle`'s forwarding arm without a
test behind the words; since row-parametricity-forwarding-law (67239be6f)
there is one, and the spec's Decisions now point at TestRowForwarding and
specs/row-parametricity-forwarding-law.md.
