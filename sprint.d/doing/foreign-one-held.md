- [ ] foreign-one-held — stage 2c of specs/foreign-one.md: `hold`, `method`
      and `attr` fold into `call{fn: address, args, held}`, the address a
      name (`"mod:fn"`) or a held object's method or attribute
      (`{"ref": r, "method": m}`, `{"ref": r, "attr": a}`), `held: true`
      keeping the answer on the far side as a ref. Python, TypeScript, R.
      After foreign-one-program.
