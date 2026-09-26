## book-delivery-prices - chapter 16b: prices(shop) and delivery(shop)

Team A's helper is `prices(shop)`, every item a shop sells with its
price (`EitherT[List, String, (String, Int)]` in cats), and team B's is
`delivery(shop)`; the item parameter is gone and the `for` binds
`(item, price)` in all three versions. The refusal and the per-shop
answers are unchanged and still asserted (TestBookTwoMonadsCats 14).
