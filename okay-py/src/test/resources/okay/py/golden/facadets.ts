import type { Order, Totals } from "./model.ts";

export interface Receipt {
  lines: string[];
  total: number;
  note: string | null;
}

export function total(order: Order): Totals {
  return { sku: order.sku, amount: 3 * order.qty, note: order.gift };
}

export async function receipt(items: string[], each: number) {
  const r: Receipt = { lines: items, total: items.length * each, note: null };
  return r;
}

export function greet(name: string, punct?: string) {
  return "hello " + name + (punct ?? "!");
}

export function echo(x: unknown): unknown {
  return x;
}

export function first<T>(xs: T[]): T {
  return xs[0];
}

export function twice(xs: number[], f: (x: number) => number): number[] {
  return xs.map((x) => f(f(x)));
}

export const version = "1";
