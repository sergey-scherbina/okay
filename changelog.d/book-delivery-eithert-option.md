## book-delivery-eithert-option - chapter 16b: team B's stack is EitherT over Option

Team B's delivery helper is now `EitherT[Option, String, Int]` (None: the
shop does not deliver; Left: an unknown shop), so both teams write
`EitherT[_, String, _]` and differ only in the monad inside — List for
team A, Option for team B. The one-expression `for` (in braces, price
then fee) is refused with a single, pinned error: `Found:
EitherT[Option, String, (String, Int)]`, `Required: EitherT[List, AA, D]`.
The hand conversion into team A's stack folds the Option layer into a
value. The three versions still agree (TestBookTwoMonadsCats 14).
