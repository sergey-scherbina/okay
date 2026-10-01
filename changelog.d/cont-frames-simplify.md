## cont-frames-simplify - Cont's runner part tidied after the move onto the frame machine

- The head form's value is read in one place (`answerOf`: `run`,
  `value`, `enter` each spelled the `Return` match and its error);
  the strict `k`'s three save/set/restore branches are one `nested`;
  the run's stack gauge is made on demand by `Root.gaugeNow`;
  `opaqueLeaf` is folded into `shiftLeaf`. `Delim.samePrompt`'s doc
  says the machine compares by `eq` now. No behaviour change; the
  affected gate green. 1bec6586a.
