- [ ] parquet-nested — nested Parquet columns (operator, 2026-09-26).
      okay-parquet reads and writes flat schemas and refuses a group or a
      REPEATED column by name. Needs: Dremel repetition and definition
      levels over okay-arrow's `ListOf` and `Struct`, both ways, and the
      DELTA_* encodings on read. Gate: pyarrow's and parquet-java's
      nested files read equal to what they wrote; ours read by both.
