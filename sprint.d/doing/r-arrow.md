- [ ] r-arrow — frames as Arrow files/streams once the JSON-frame
      road hurts. MEASURED 2026-09-09 (r-measure-harden,
      `MeasureRFrame`, medians of five against the dockerized R 4.4.1,
      `identity` on a 3-column frame):

      | rows | payload | our encode | round trip | our decode | typed rows | OUR share |
      |---|---|---|---|---|---|---|
      | 10 000 | 0.30 MB | 6.5 ms | 1 546 ms | 7.3 ms | 2.2 ms | 0.9% |
      | 100 000 | 3.21 MB | 20.4 ms | 13 686 ms | 18.3 ms | 6.0 ms | 0.3% |

      The number says the opposite of the Python twin's. There, 60% of
      the trip was OUR parser; here our two halves are 0.3% and the
      other 99.7% is R. Nor is it the pipe: 3.21 MB in 13.7 s is
      ~230 KB/s, and a pipe does that in milliseconds — we encode and
      decode the same bytes at ~83 MB/s. So the cost is jsonlite
      walking the STRUCTURE we hand it, and the structure is the
      suspect below. Arrow would still remove it, at the price of the
      `arrow` package (native, heavy) on R's side and an Arrow reader
      on ours — a big dependency for a module whose only dependency
      today is jsonlite. Try the cheap shape change first.
      HALF BUILT by py-arrow (2026-09-25): the JVM side exists —
      `okay.arrow.OkayArrow` (okay-arrow, a cross module since 2026-09-25) writes and reads Arrow IPC streams (int64,
      float64, utf8, bool, null; nullable; pyarrow-validated), and the
      wire carries a frame as ONE Arrow stream with the request header in
      its metadata (`FrameFormat` givens, `frames: ["arrow"]` in the
      hello). What R needs is only its half: the shim announcing arrow
      when the `arrow` package is installed and `RSubprocess` taking
      `FrameFormat`. R's `arrow::read_ipc_stream`/`write_ipc_stream` do
      the rest. Python's measurement for scale: 500k rows 806 -> 147 ms.
