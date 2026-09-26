## facade-frame-seam — the Arrow cells of the measurement table (2026-09-26)

The last of facade-frame-seam: its Arrow cells, which waited for an
interpreter with pyarrow. pyarrow 25.0.1 now lives in a uv venv
(`~/.local/share/okay/pyarrow-venv`, `OKAY_PYARROW_PYTHON`), beside a
system python3 without it so the JSON lanes still have their arm. On Arrow,
100 000 rows through the facade take 51–54 ms (157–170 ms on columnar
JSON), a `Table` through `Frames.frame` 11.5–12.5 ms (124–130 ms), and the
facade equals the own road. MeasureRFrame gained an Arrow lane: R, 100 000
rows, 61.8 ms against 169.5 ms. The Rust in-process cell is
foreign-arrow-ffm's 20.7 ms against 119.8 ms. specs/foreign-facade.md.
