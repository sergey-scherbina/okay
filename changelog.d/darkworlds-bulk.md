## darkworlds-bulk - Dark Worlds read and observed as a Bulk

Operator: "why Vectors in Dark Worlds, not streams or better Bulk?" — it
was written one lane before `observeBulk` existed. Now a sky is read by
`Bulk.csv`, mapped to galaxies and cached (`DarkWorlds.galaxies`), and both
model forms observe it by `observeBulk` (`Bayes` for adaptive, `Smooth`
for AD NUTS), written over any `Bulk[D]`; the test runs them on
`Bulk[Chunks]` reading the resources. The grid oracle keeps its own Vector
reading, and a new test holds the Bulk model's density equal to it in both
forms. Results unchanged in substance: Sky 3 by AD NUTS x 2323.9 (grid
2324.1); ten skies, the truth inside the 95% region 10 of 10, median 42.
A stream is the wrong shape for this likelihood (an unordered sum re-read
per gradient); `Online.filter` is the stream example.
