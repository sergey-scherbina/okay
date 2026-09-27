- [ ] r-arrow-timestamp-exact — shim.R's Arrow reply loses a microsecond
      on a POSIXct column: R holds seconds as a double, 536.074 is not
      exact, and arrow's POSIXct → timestamp[us] TRUNCATES, so
      536074000 µs goes out as 536073999 (okay-watch's round-trip
      property, seed 103, 2026-09-27). Fix: the reply rounds a POSIXct
      column to whole microseconds before it becomes Arrow. Proof: a
      Live test of values that are not exact doubles, red first.
