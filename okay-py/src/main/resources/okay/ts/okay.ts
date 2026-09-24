// okay's TypeScript library (specs/typescript.md, stage 1). A module the
// TypeScript worker loads imports what it needs from here:
//
//   import { call, done, perform, then } from "./okay.ts";
//
// Written in the TypeScript Node runs by STRIPPING types (no enums, no
// parameter properties, no namespaces), so there is no build step.

/** a program as data: an answer, or a named operation and the function
 * that continues with its answer. The continuation is a plain function,
 * so okay may continue it more than once (a Choice handler does). */
export type Prog<A, O extends Ops = Ops> =
  | { readonly tag: "done"; readonly value: A }
  | {
      readonly tag: "perform";
      readonly name: keyof O & string;
      readonly args: readonly unknown[];
      readonly k: (answer: any) => Prog<A, O>;
    };

/** the operations a program may perform, as a record of signatures —
 * its effects, in its type (typescript-types T12). `Ts.ops` writes one
 * from the Scala callbacks' Schemas. The default, any name, is the
 * untyped program. */
export type Ops = Record<string, (...args: any[]) => unknown>;

/** a program that has answered `value` */
export function done<A>(value: A): Prog<A> {
  return { tag: "done", value };
}

/** ask okay to run the operation `name` (a callback the Scala side
 * offered); the program continues with its answer */
export function perform<R = any>(name: string, ...args: unknown[]): Prog<R> {
  return { tag: "perform", name, args, k: (answer: R) => done(answer) };
}

/** the program `p`, then `f` of its answer */
export function then<A, B, O extends Ops = Ops>(p: Prog<A, O>, f: (a: A) => Prog<B, O>): Prog<B, O> {
  if (p.tag === "done") return f(p.value);
  const k = p.k;
  return { tag: "perform", name: p.name, args: p.args, k: (x: any) => then(k(x), f) };
}

/** a callback that failed in okay: its condition's kind and message */
export class OkayError extends Error {
  readonly kind: string;
  constructor(kind: string, message: string) {
    super(`${kind}: ${message}`);
    this.name = "OkayError";
    this.kind = kind;
  }
}

/** the channel the worker installs; `okay_call` goes through it */
export const hooks: { call?: (name: string, args: unknown[]) => unknown } = {};

/**
 * Call back into okay in the middle of a call: the callback `name` the
 * Scala side offered to THIS call runs as an okay program under the
 * caller's handlers, and its answer is the value here. Synchronous: the
 * function simply returns it.
 */
export function okay_call<R = unknown>(name: string, ...args: unknown[]): R {
  if (!hooks.call) throw new Error(`okay_call("${name}") outside a call okay started with callbacks`);
  return hooks.call(name, args) as R;
}

/** `okay_call`'s old name, kept */
export const call = okay_call;

/** a program's operations, typed by `O`: a name `O` does not have, or
 * arguments or an answer of the wrong type, do not compile. The combinator
 * is `andThen`, not `then`: an object with a `then` method is a thenable,
 * and `await` would try to resolve it. */
export interface Effects<O extends Ops> {
  perform<K extends keyof O & string>(name: K, ...args: Parameters<O[K]>): Prog<ReturnType<O[K]>, O>;
  call<K extends keyof O & string>(name: K, ...args: Parameters<O[K]>): ReturnType<O[K]>;
  andThen<A, B>(p: Prog<A, O>, f: (a: A) => Prog<B, O>): Prog<B, O>;
  done<A>(value: A): Prog<A, O>;
}

/** `effects<ShopOps>()`: perform, call, andThen and done, typed by the operations */
export function effects<O extends Ops>(): Effects<O> {
  // the functions are the untyped ones above; the record type is the
  // claim, and every use site is where tsc checks it
  return { perform, call, andThen: then, done } as unknown as Effects<O>;
}
