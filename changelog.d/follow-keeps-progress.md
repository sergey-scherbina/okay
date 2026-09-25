## follow-keeps-progress - a source that fails mid-step no longer takes the confirmed blocks with it

Found from okay-watch (its tron-keyless-rate, 2026-09-25): a `PollSource`
that threw in the middle of `Follow.step` — a 429 after the retries — took
the step's events with it, blocks the Tracker had already confirmed and no
caller received: transfers missed in silence. okay-watch's old Follower had
the same hole.

- `Follow.step` ends with what it confirmed when the source fails after
  progress; the poller asks the same height again, and a failure that gains
  nothing is thrown.
- TestWatchShape: "a failure after progress keeps what was confirmed; the
  height is asked again next step". specs/chain.md.
- Gate: okay-chain on JVM/JS/Native 75; `affected master` 237, GREEN.

Landed as a9759ba29.
