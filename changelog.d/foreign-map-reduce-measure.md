## foreign-map-reduce-measure - the map and reduce in Python against Scala, JSON against Arrow

The number foreign-map-reduce and foreign-reduce landed without (the
operator's plan, item 5). `MeasureForeignMapReduce` (Live): 1M rows, 4
partitions, medians of three, the JSON lane on the box's python3 and the
Arrow lanes on a venv of the same interpreter with pyarrow 25.0.1.

- At box load 8–13 (two runs, within ~10%): Scala map 6–12 ms; Python
  map ~200 ms over JSON, ~95 ms over Arrow, ~86 ms under `@okay.arrow`
  with `pyarrow.compute`; the reduce in Python on top ~+70 ms. Three
  in-process workers cost nothing visible over the fan.
- A run at load 21–27 read 2–3x slower on every lane and showed a
  batch-size effect the quiet runs do not: discarded, and the batch stays
  at 4096.
- Verdict, in specs/foreign-map-reduce.md: Arrow halves the Python map;
  a Python map is ~10x a Scala map at 90 ms per million rows, all of it
  Python's own work and the pipe; move the reduce across only for a
  reduction the JVM has not got.
