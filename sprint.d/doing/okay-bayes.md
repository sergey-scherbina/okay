- [ ] okay-bayes — operator ask 2026-10-02 ("okay-bayes without Python",
      Bayesian Methods for Hackers; specs/okay-bayes.md). Stage 1 (this
      lane): module okay-bayes — distributions with logPdf/sample/propose/
      coerce, the `Model` effect (named `sample`, `observe` as a factor),
      prior / likelihood weighting / lightweight Metropolis–Hastings,
      summaries (HDI, ESS, R-hat); oracles: conjugate posteriors, the
      book's ch.1 texting model on its data, PyMC via okay-py (Live).
      Stages 2–4 (SMC, HMC/NUTS, the Inference facade) after it.
