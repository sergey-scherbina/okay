- [ ] dataflow-rescale-windowed — stage 13 box 2: journal a windowed
      operator's OPEN panes so a re-cut can rescale it, rather than
      relying on replay (which a re-cut cannot do). Found by
      dataflow-rescale: today a windowed rescale is refused, naming
      this. The boundary panes would be re-bucketed under the new
      extents on resume.
