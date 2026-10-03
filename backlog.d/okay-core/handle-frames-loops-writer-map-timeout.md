- [ ] handle-frames-loops-writer-map-timeout — PRIORITY: LOW, a flake
      sighting (symdex-okay's gate, 2026-10-03). TestHandleFramesLoops
      "Writer.map" timed out at 45 s (munit's 30 s limit) in a full
      `affected master staged` run at load 25-48; alone on the same tree
      it passed in 0.83 s. A 55x slowdown under contention is more than
      load alone usually explains for a test this size — worth a look at
      what Writer.map's nested-depth case does when the box is busy
      (a thread handoff, a spin?) before it is called a plain flake.
