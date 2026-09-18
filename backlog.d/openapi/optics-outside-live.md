- [ ] optics-outside-live — subscribe to a lens. The lens compiles to
      a wire path, the server pushes only the focused part of the
      document, and a client write comes back as `set`. The most
      valuable of the six to a user and the most work: the optic must
      be reifiable and must survive serialisation. okay-live,
      okay-persist, okay-crdt.
      MEASURED 2026-09-11 at the only live consumer in the tree, and
      it is already minimal: `ChatDemo` publishes a KIND
      (`feed.publish("board")`) and the client re-fetches
      `/board.json`. A lens-addressed delta would replace a cheap
      re-fetch of a handful of tasks. TRIGGER: a document large enough
      that re-fetching it on every change is the measured cost — with
      the measurement, not the intuition.
