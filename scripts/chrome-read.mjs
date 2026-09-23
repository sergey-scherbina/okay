// Open a page in headless Chrome and print the text of one element once
// the page has put something other than its placeholder there — through
// the DevTools protocol, because `--dump-dom` dumps at the load event
// (before a module's async work) and hung outright under
// `--virtual-time-budget` with IndexedDB pending (typescript-types T10).
//
//   node scripts/chrome-read.mjs <chrome> <profile-dir> <url> <selector> <placeholder> [timeout-ms]
//
// Prints the text; exits 1 with a reason when it does not come.
import { spawn } from "node:child_process";
import { readFile, rm } from "node:fs/promises";
import { setTimeout as sleep } from "node:timers/promises";

const [chrome, profile, url, selector, placeholder, limit = "30000"] = process.argv.slice(2);
const until = Date.now() + Number(limit);
// a second load of the same profile would read the FIRST browser's port:
// the file outlives the browser that wrote it
await rm(`${profile}/DevToolsActivePort`, { force: true });
const browser = spawn(chrome, ["--headless=new", "--disable-gpu", "--no-first-run", "--no-default-browser-check",
  "--remote-debugging-port=0", `--user-data-dir=${profile}`, "about:blank"], { stdio: "ignore" });
const fail = (why) => { console.error(why); browser.kill(); process.exit(1); };

// Chrome writes the port it chose into the profile
let port;
while (!port) {
  if (Date.now() > until) fail("Chrome never wrote DevToolsActivePort");
  try { port = (await readFile(`${profile}/DevToolsActivePort`, "utf8")).split("\n")[0]; } catch { await sleep(100); }
}
const targets = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
const page = targets.find((t) => t.type === "page");
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise((ok) => ws.addEventListener("open", ok, { once: true }));
let id = 0;
const pending = new Map();
// what the page said on its console and what it threw: printed when the
// text never comes, because "still waiting" alone names no cause
const said = [];
ws.addEventListener("message", (m) => {
  const msg = JSON.parse(m.data);
  if (msg.id && pending.has(msg.id)) { pending.get(msg.id)(msg); pending.delete(msg.id); }
  if (msg.method === "Runtime.exceptionThrown") said.push("threw: " + (msg.params.exceptionDetails.exception?.description ?? msg.params.exceptionDetails.text));
  if (msg.method === "Runtime.consoleAPICalled") said.push(msg.params.type + ": " + msg.params.args.map((a) => a.value ?? a.description).join(" "));
  if (msg.method === "Log.entryAdded") said.push("log: " + msg.params.entry.text);
});
const send = (method, params = {}) => new Promise((ok) => { id += 1; pending.set(id, ok); ws.send(JSON.stringify({ id, method, params })); });

await send("Runtime.enable");
await send("Log.enable");
await send("Page.navigate", { url });
let text = placeholder;
while (text === placeholder || text === undefined) {
  if (Date.now() > until) fail(`${selector} still says ${JSON.stringify(text)} after ${limit} ms\n${said.join("\n")}`);
  await sleep(200);
  const r = await send("Runtime.evaluate", { expression: `document.querySelector(${JSON.stringify(selector)})?.textContent`, returnByValue: true });
  text = r.result?.result?.value;
}
// close the browser cleanly, so the profile (IndexedDB, localStorage) is flushed for the next load
await send("Browser.close");
await new Promise((ok) => browser.on("exit", ok));
console.log(text);
