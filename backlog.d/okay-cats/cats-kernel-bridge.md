- [ ] cats-kernel-bridge — cats' `Semigroup`/`Monoid` and okay's are two
      classes (Fold.scala), so `catsValidated` asks OUR `Semigroup` and
      `okayCatsValidated` asks CATS': a user holding one has to write the
      other. Bridge both ways in okay-cats, behind imports that cannot
      loop (the FromCats/ToCats rule), plus `Eq` where a test wants it.
      Found by the cats-depth audit, 2026-10-02 (operator: "делай все это").
