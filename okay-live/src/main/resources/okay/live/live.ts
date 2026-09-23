// okay-live's TypeScript client (typescript-types T11): subscribe to the paths of
// a Watched document served by okay.live.LiveHttp. `P` is the <Name>Paths
// interface okay.codec.Stubs.typescriptPaths writes from the document's Scala
// Schema, so a key the schema has no place for does not compile, and each
// value arrives with the type of what that key focuses.

export interface LiveOptions {
  base: string;
  prefix?: string;
  fetch?: typeof fetch;
}

/** a key the server refused, or a subscription that failed, with the reason */
export class LiveRefused extends Error {
  readonly key: string;
  constructor(key: string, why: string) {
    super(`${key}: ${why}`);
    this.name = "LiveRefused";
    this.key = key;
  }
}

export interface Live<P> {
  /** the focused value now, then each time it changes; answers the unsubscribe */
  watch<K extends keyof P & string>(key: K, f: (value: P[K]) => void, refused?: (e: LiveRefused) => void): () => void;
  /** write the focused value */
  set<K extends keyof P & string>(key: K, value: P[K]): Promise<void>;
}

export function live<P>(o: LiveOptions): Live<P> {
  const send = o.fetch ?? fetch;
  const at = `${o.base}/${o.prefix ?? "live"}`;
  return {
    watch(key, f, refused) {
      const stop = new AbortController();
      const read = async () => {
        const r = await send(`${at}/watch?key=${encodeURIComponent(key)}`, { signal: stop.signal });
        if (!r.body) throw new Error("no event stream");
        const events = r.body.pipeThrough(new TextDecoderStream()).getReader();
        let buffer = "";
        for (;;) {
          const { value, done } = await events.read();
          if (done) return;
          buffer += value;
          let end = buffer.indexOf("\n\n");
          while (end >= 0) {
            const lines = buffer.slice(0, end).split("\n");
            buffer = buffer.slice(end + 2);
            const name = lines.find((l) => l.startsWith("event: "))?.slice(7) ?? "message";
            const data = lines.filter((l) => l.startsWith("data: ")).map((l) => l.slice(6)).join("\n");
            if (name === "refused") refused?.(new LiveRefused(key, JSON.parse(data)));
            else f(JSON.parse(data));
            end = buffer.indexOf("\n\n");
          }
        }
      };
      read().catch((e) => { if (!stop.signal.aborted) refused?.(new LiveRefused(key, String(e))); });
      return () => stop.abort();
    },
    async set(key, value) {
      const r = await send(`${at}/set`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ key, value }),
      });
      if (!r.ok) throw new LiveRefused(key, await r.text());
    },
  };
}

/** `<okay-live base="…" key="…">`: its text is the focused value, kept current.
 * No framework: a custom element, defined once, in a browser only. */
export function defineElement(name = "okay-live"): void {
  if (typeof customElements === "undefined" || customElements.get(name)) return;
  customElements.define(name, class extends HTMLElement {
    private stop: (() => void) | undefined;
    connectedCallback() {
      const client = live<Record<string, unknown>>({ base: this.getAttribute("base") ?? "", prefix: this.getAttribute("prefix") ?? "live" });
      this.stop = client.watch(this.getAttribute("key") ?? "", (v) => {
        this.textContent = typeof v === "string" ? v : JSON.stringify(v);
      });
    }
    disconnectedCallback() {
      this.stop?.();
    }
  });
}
