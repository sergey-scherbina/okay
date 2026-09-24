"use strict";
const prices = { tea: 4.0, cake: 2.5 };

function priceOf(sku) {
  if (!(sku in prices)) throw new RangeError("no price for " + sku);
  return prices[sku];
}

class Cart {
  constructor() { this.items = []; }
  add(item) { this.items.push(item); return this; }
  get size() { return this.items.length; }
  total(price) { return this.items.reduce((s, i) => s + price(i.sku) * i.qty, 0); }
}

function fetchRate(currency) {
  const rates = { EUR: 1.0, UAH: 45.0 };
  return new Promise((resolve, reject) =>
    setTimeout(() => currency in rates ? resolve(rates[currency]) : reject(new Error("no rate for " + currency)), 5));
}

module.exports = { priceOf, Cart, fetchRate };
