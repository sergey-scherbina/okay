(ns okay.clojure.quote
  "A Clojure program performing the caller's callbacks (foreign-one-ops):
  okay.clojure.shop is generated from the Scala Foreign.callbacks that serve
  every wire language the same two operations."
  (:require [okay.core :as ok]
            [okay.clojure.shop :as shop]))

(defn quote-of
  "the price of qty of sku, after the discount"
  [sku qty]
  (ok/mlet [price (ok/perform (shop/price-of sku))]
    (ok/perform (shop/discount (* price qty)))))

(defn wrong
  "a price asked for a number: the callback's Schema refuses it by name"
  []
  (ok/perform (shop/price-of 42)))
