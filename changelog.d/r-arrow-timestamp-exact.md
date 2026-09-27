## r-arrow-timestamp-exact - a timestamp crosses R to the microsecond

- R keeps a POSIXct as double seconds, and arrow's POSIXct to
  `timestamp[us]` conversion truncates, so a value that is not an
  exact double came back one microsecond short (536074000 µs came back
  as 536073999). okay-watch's round-trip property found it.
- shim.R's `okay_arrow_reply` now rounds each POSIXct column to whole
  microseconds and casts it through int64, keeping the column's time
  zone.
- `TestRArrowTimestamp` (Live) sends 1005 values and a null, and all
  come back exactly. It was red first, with 21 of 1005 changed.

Spec: specs/r.md "r-arrow-timestamp-exact".
