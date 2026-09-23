// A React hook over okay-live's client (typescript-types T11): the focused value
// of a key, re-rendered on every change, unsubscribed on unmount.
import { useEffect, useState } from "react";
import type { Live } from "./live.ts";

export function useWatch<P, K extends keyof P & string>(client: Live<P>, key: K): P[K] | undefined {
  const [value, setValue] = useState<P[K] | undefined>(undefined);
  useEffect(() => client.watch(key, (v) => setValue(() => v)), [client, key]);
  return value;
}
