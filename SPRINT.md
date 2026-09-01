# Sprint

The short-term queue agents pick from. The claim files in
`.work/active/` are authoritative for who is working on what right now;
this file is what is worth doing next, in order.

## Doing
- [ ] cluster-nio — `specs/http-backends.md` deferred it explicitly:
      "Replacing okay-cluster's transport. NIO makes it possible and
      that is a separate change with its own measurements."
      `cluster.Remote` holds blocking sockets and line-delimited JSON;
      `Nio.Conn` parks nothing. Measure first — on Java 21 the answer
      may be that Loom parking already costs nothing, and that is a
      result worth recording rather than a change worth making.

## Queue
- [ ] symbol-fold-cost — `Symbols.of` is 221.5us of a 628.7us index and
      nobody knows where it goes. The obvious answer was measured and
      REFUTED (rebuilding the Index per token bought 0). The diagnostic
      lane exists (`indexFoldNoRefs`) but returned ±136 noise on a
      loaded machine; on a quiet one this is one run.
- [ ] ws-close-halfduplex — `specs/http.md` leaves this unchecked
      honestly: what is shown is that a Text then a Close both come
      back in order, not the stronger claim that frames in flight after
      a Close still arrive. Needs a fixture that keeps sending after
      receiving Close, which `WsEcho` does not do.
