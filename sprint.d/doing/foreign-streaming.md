- [ ] foreign-streaming — a Python generator (or an R function over
      chunks) as an okay `Stage`: `Py.stage[I, O]("mod:gen")`, elements
      crossing in okay `Chunks` (one message per chunk, not per element),
      pull-driven so a slow model back-pressures the source. The
      batch-inference shape: an okay stream feeding a PyTorch model and
      reading predictions back without materialising either side. The
      far generator is one-shot; a second run starts a new generator.
