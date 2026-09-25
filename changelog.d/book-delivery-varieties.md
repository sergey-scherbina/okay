## book-delivery-varieties - chapter 16b: a price comes with the variety it is for

Team A's helper in the delivery example returned a bare `Int` from a
`List`, which left the list unexplained. It now reads a catalog where a
shop sells several varieties of an item, and answers `(variety, price)`:
`EitherT[List, String, (String, Int)]` in cats, `Either[String,
List[(String, Int)]]` for layered reflection, `(String, Int) ! Choose +
Throws % String` with effects. The order for "tea" over both shops is
`List(Right(Some(("green tea", 350))), Right(Some(("black tea", 330))),
Right(None))` in all three, asserted; the one-expression refusal was
recaptured and re-pinned.
