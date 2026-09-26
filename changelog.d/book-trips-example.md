## book-trips-example - chapter 16b: a two-leg trip, where monad composition is not optional

The "stacks do not compose" example is now a trip with a change of
plane, replacing the delivery example (whose List was really data: the
logical answer there was an error or a list, which needs no
transformer). Here the second flight depends on where the first landed
(List as a monad: a tree, not a precomputed list), a leg can fail and
only that itinerary fails (Either per branch), and each itinerary keeps
its own booking log (Writer per branch).

- Team A: `flights(from): EitherT[List, String, (String, Int)]`; team B:
  `book(from, to): EitherT[Writer[Vector[String], *], String, Unit]`. The
  one-for trip is refused at every switch between the teams (the first
  error pinned); the union `EitherT[WriterT[List, …]]` with `fromA`/`fromB`.
- Layered reflection: List, Logged and Either layers, helpers reflecting
  through their capabilities, the same `for`.
- Effects: `Choose + Writer % String + Throws % String`, the same `for`,
  handlers giving each itinerary its own error and log.
- All three give the same three itineraries from A (TestBookTwoMonadsCats
  13); the delivery objects and their tests are removed.
