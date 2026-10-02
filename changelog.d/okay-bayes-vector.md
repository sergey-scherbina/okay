## okay-bayes-vector - vectors as named scalars

okay-bayes, the spec's open question answered (specs/okay-bayes.md §6): a
vector of parameters is n named scalars, so every sampler (MH, adaptive,
NUTS, AD NUTS, SMC) takes it unchanged. `Bayes.sampleN(name, d, n)` and
`Smooth.paramN(name, prior, n)` draw `name[0]` … `name[n-1]`;
`Posterior.vector(name)` reads them back per element. TestVector: eight
schools (Rubin 1981) with τ fixed — jointly Gaussian, so exact — by
adaptive Metropolis (μ 4.33, exact 4.34) and by AD NUTS (μ 4.27), every θ
within error. A distribution over vectors as ONE site (Dirichlet, LKJ)
would need a vector-valued trace and a simplex transform: not built.
