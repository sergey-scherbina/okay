- [ ] gate-fanout-what-is-left — the bound took the peak from 165 to
      104, not to the ~14 one task per core would predict. What the
      rest IS, is unmeasured: link steps, the gtk test, whatever else
      spawns beside the runners. Sample the tree BY COMMAND during one
      quiet full gate and name what makes up the 104. Key the sampler
      on the descendants of ONE pid — the first cut matched `^node$`
      and reported a peak that was mostly a sibling's build.
