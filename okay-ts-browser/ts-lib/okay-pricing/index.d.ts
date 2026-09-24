/** a line of a cart */
export interface Item {
  sku: string;
  qty: number;
}

/** the catalogue price of a product; throws for an unknown one */
export declare function priceOf(sku: string): number;

/** a cart: items in, a total out, priced by the function it is given */
export declare class Cart {
  constructor();
  add(item: Item): Cart;
  readonly size: number;
  total(price: (sku: string) => number): number;
}

/** an exchange rate, from "the network": a Promise, as a browser API answers */
export declare function fetchRate(currency: string): Promise<number>;
