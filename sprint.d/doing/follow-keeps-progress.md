- [ ] follow-keeps-progress — found from okay-watch (2026-09-25): when a
      PollSource throws in the middle of `Follow.step` (a 429 after the
      retries, a timeout), the step's exception discards the events it had
      already produced — blocks the Tracker had CONFIRMED (its frontier
      moved) and that no caller ever received: transfers missed in
      silence. The step now returns what it confirmed when the source
      fails after progress; the failure surfaces on a step that gains
      nothing (the poller asks the same height again, so nothing is
      skipped). okay-watch's old Follower had the same hole.
