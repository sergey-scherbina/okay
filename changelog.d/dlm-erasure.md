## dlm-erasure - a person's own words out of the ledger, the fact of it kept

specs/dlm-learning.md §10. Found by okay-watch's check bot, the first
consumer whose model learns from the people who use it: a lesson IS the
person's sentence in our ledger, §9's `Retention` drops old TABLES, and
nothing took a person's words back out. Append-only and the right to
have one's data removed reconcile one way — the content goes, the fact
stays.

- `Ledger.Entry.Erased(subject, entries, why, at, by)`, and it is never
  itself erased: a ledger that can lose that record cannot show an
  erasure ever happened.
- `Ledger.Erasable` (`Recorded`, and `File` by an atomic move, dropping
  lines it could not parse — a line whose subject is unknown is what an
  erasure may not leave behind); `Ledger.erase` pure, over the entries
  that NAME a person; `Ledger.digest` for a subject that is not the
  person.
- `Governed.erase(by, who, why, subject)`: the rights `Teaching` already
  had (oneself, or a steward), NOT gated by the kill switch — a system
  that cannot learn must still be able to forget somebody — the memory
  re-folded from what the ledger now holds, and this value's own history
  erased too.
- Three tests. Two design decisions came out of them failing: a person
  erasing themselves is recorded as the subject and not by name (`by`
  had kept the identifier the digest was there to remove), and only that
  person leaves the memory when the sink cannot erase.
