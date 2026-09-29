## dlm-hash-platform - «the same corpus hashes the same» holds on one platform, and now says so

Measured by the first consumer (Okay!Chat, 2026-09-29): the same corpus
and labels through the same int8 encoder on another CPU gave vectors at
cosine 0.97–0.99 of the committed ones, and every table a new hash.
`Exemplars.hash`'s comment, the module page and specs/dlm-learning.md
§9 claimed two builds of one corpus under one encoder hash the same;
they now say ON THE SAME PLATFORM, and that a `Rebuilt` whose corpus
did not move names a platform rather than a mistake. The consumer's
answer is to compile its tables where they are served — its image
build already does. Comments and prose only; no behaviour moved.
