- [ ] backup-active-segment — `Backup.copy` leaves the active segment
      home, so a backup is bounded by `segmentBytes` of unsaved books.
      A shop wants "everything up to now": copy the active segment
      under a distinct key (it changes, so it is not incremental) or
      roll on demand before a backup. Found by one-binary-story.
