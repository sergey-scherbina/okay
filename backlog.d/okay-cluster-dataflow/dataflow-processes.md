- [x] dataflow-processes — LANDED as stage 4. Jobs by NAME with
      Schema'd parameters, a four-byte length and CBOR, partials back;
      `Job`, `Jobs`, `Req`/`Resp`, `Served`, `WorkerMain`. The
      acceptance ran twice: the synthetic feed across four real OS
      processes in stage 4b (TestDistributed), and the full Wrocław
      Result across four of them in stage 7 (MeasureWroclawCluster),
      which is the one this entry asked for.
