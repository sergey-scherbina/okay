## acmc-docs-followup — merge docs no longer call the fixed chunked-merge cost open

docs/merge-and-wait.md and specs/ready-merge.md named
`adaptive-chunked-merge-cost` as open after it landed (851d59dd5); both
now give the fixed number and point at the one gap left,
`adaptive-elementwise-small-ring`.
