## okay-bayes-sampler - the sampler as a facade, PyMC behind an import

okay-bayes stage 4 (specs/okay-bayes.md, specs/own-or-standard.md):
`Sampler` (cross), ours the default given (`Sampler.Okay`, `Nuts.sample`);
`Bayes.nuts` and `Smooth.nuts` take the sampler in scope. `PyMC` (JVM,
`import okay.bayes.PyMC.given`, okay-py now an OPTIONAL dependency of
okay-bayes) runs PyMC's own NUTS on okay's `Target`: a `pm.Potential` over
a flat ℝᵈ through a black-box pytensor Op whose log density and gradient
call back into okay (`okay.call`). `PyMC.missing` names okay-py when absent;
`Samplers.byName("okay" | "pymc")`. TestPyMCSampler (Live): a conjugate in
closed form and Challenger against the grid, zero divergences.

Found: PyMC's NUTS on okay's raw-temperature Challenger density does not
diverge, which REFUTES okay-bayes-ch2's explanation (collinearity) for its
all-divergent run on the book's model; the spec, docs, test comment and
that entry are corrected. Stan needs its own language and toolchain: not
built, recorded.
