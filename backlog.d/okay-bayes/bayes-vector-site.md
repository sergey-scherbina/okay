- [ ] bayes-vector-site — specs/okay-bayes.md §6, left open when vectors
      became named scalars (okay-bayes-vector). A distribution over a VECTOR
      as one site — Dirichlet (today only a posterior helper in AbTest),
      LKJ for correlation matrices, multivariate Normal — needs a
      vector-valued trace entry and, for NUTS, a simplex transform
      (stick-breaking) and a Cholesky-factor one. Would let ch.6's stock
      returns (Wishart/LKJ priors) be written as the book does. Oracle:
      Dirichlet–multinomial in closed form through NUTS.
