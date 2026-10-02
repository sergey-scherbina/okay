- [ ] okay-bayes — operator ask 2026-10-02 ("okay-bayes without Python",
      Bayesian Methods for Hackers; specs/okay-bayes.md). Stage 1 (this
      lane): module okay-bayes — distributions with logPdf/sample/propose/
      coerce, the `Model` effect (named `sample`, `observe` as a factor),
      prior / likelihood weighting / lightweight Metropolis–Hastings,
      summaries (HDI, ESS, R-hat); oracles: conjugate posteriors, the
      book's ch.1 texting model on its data, PyMC via okay-py (Live).
      Stages 2–4 (SMC, HMC/NUTS, the Inference facade) after it.
      Stage 1 landed 53219afcb; stage 2a (ch.2: A/B, Challenger,
      adaptive Metropolis) okay-bayes-ch2; 2b SMC okay-bayes-smc; 2c ch.3
      mixture okay-bayes-ch3; 2d ch.6 Thompson okay-bayes-ch6. Next:
      stage 3: 3a NUTS + finite differences okay-bayes-nuts; 3b
      reverse-mode AD (`Real`, `Grad`, `Smooth`) okay-bayes-ad. Stage 4,
      the `Sampler` facade with PyMC behind an import, okay-bayes-sampler.
      The spec's stages are done; what remains is specs/okay-bayes.md §6.
