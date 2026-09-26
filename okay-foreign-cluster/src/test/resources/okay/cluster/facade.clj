(ns okay.cluster.facade
  "Clojure programs the facade runs (foreign-jvm-programs): the conformance
  body's `priced` and `pairs`, performing the caller's callbacks by name."
  (:require [okay.core :as ok]))

(defn priced
  "an order's price: `price_of` asked of the caller, times the quantity"
  [order]
  (ok/mlet [p (ok/perform (okay.clojure.Ops/call "price_of" (get order "sku")))]
    (ok/done (* p (get order "qty")))))

(defn pairs
  "two choices, each a callback the caller answers as often as Choice asks"
  [_]
  (ok/mlet [x (ok/perform (okay.clojure.Ops/call "choose" [1 2]))
            y (ok/perform (okay.clojure.Ops/call "choose" [10 20]))]
    (ok/done (+ x y))))
