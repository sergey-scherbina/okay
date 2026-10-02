## handle-many - several handlers at once

Level 1 (operator's roadmap, 2026-10-02). `p.handle(State(5),
Throws.either)` and three at once are `p.handle(h1).handle(h2)…`,
innermost first, each step's rest inferred from the one before.
TestHandleMany.
