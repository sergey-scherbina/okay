- [x] dataflow-rescale — stage 13: change the partition count between
      two epochs. LANDED (box 1 + box 3, TestRescale): a striped source
      (`Flow.striped`, `Job.rescalable`) into a keyed/fold sink resumes
      at a new width with the batch answer; the workers vector may
      change too. Two conditions the build now ENFORCES: the source
      must be striped (a contiguous cut has no global prefix) and the
      sink must keep its state in the fold (a windowed sink's open
      panes are not journalled). Both refuse rather than fake it.
