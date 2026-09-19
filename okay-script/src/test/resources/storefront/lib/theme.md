---
exports:
  - storefrontStyle
route: false
---

# the theme

The storefront's own look, carried over from the file the live sites
are rendered by. Two themes in one function, chosen by a VALUE rather
than copied: a warm atelier (woven texture, dashed seams, a serif
headline) and a dark systems-reader (a night gradient, a skyline, a
sweeping signal). This is what "one library, two sites" buys.

```scala declare
def storefrontStyle(clothing: Boolean, accent: String): String =
  val themeRoot =
    if clothing then """--void: #dcc4b6; --ink: #3a2a22; --mist: #7a5a50; --line: rgba(58,42,34,.14); --hsh: none; --surface: #efe3dc; --field: #fbf7f4; --scrim: rgba(58,42,34,.45); --accent-text: #9e1042;"""
    else """--void: #05070c; --ink: #eaf1fb; --mist: #76859e; --line: rgba(180,200,230,.08); --hsh: 0 0 44px rgba(120,150,200,.14); --surface: #0c1420; --field: #080d16; --scrim: rgba(2,4,8,.74); --accent-text: var(--accent);"""
  val themeBg =
    if clothing then """.bg { position: fixed; inset: 0; z-index: 0; pointer-events: none; overflow: hidden; background-color: #dcc4b6; } .bg::before { content: ''; position: absolute; inset: -50%; transform: rotate(28deg); background-image: linear-gradient(135deg, rgba(120,86,68,.13) 25%, transparent 25%), linear-gradient(225deg, rgba(120,86,68,.13) 25%, transparent 25%), linear-gradient(315deg, rgba(120,86,68,.13) 25%, transparent 25%), linear-gradient(45deg, rgba(120,86,68,.13) 25%, transparent 25%); background-size: 50px 50px; background-position: -25px 0, -25px 0, 0 0, 0 0; } .bg::after { content: ''; position: absolute; inset: 0; mix-blend-mode: multiply; opacity: .42; background-image: url("data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='150' height='150'><filter id='g'><feTurbulence type='fractalNoise' baseFrequency='0.8' numOctaves='2' stitchTiles='stitch'/><feColorMatrix type='saturate' values='0'/></filter><rect width='150' height='150' filter='url(%23g)'/></svg>"); background-size: 150px 150px; } .seam { position: absolute; left: -12%; right: -12%; height: 6px; z-index: 1; border-top: 1.5px dashed rgba(120,86,68,.48); border-bottom: 1.5px dashed rgba(120,86,68,.48); } .seam.s1 { top: 30%; transform: rotate(-7deg); } .seam.s2 { top: 30%; margin-top: 7px; transform: rotate(-7deg); } .seam.s3 { top: 70%; transform: rotate(-7deg); } .glow { display: none; }"""
    else """.bg { position: fixed; inset: 0; z-index: 0; pointer-events: none; overflow: hidden; background: radial-gradient(130% 95% at 50% 36%, transparent 50%, rgba(2,3,7,.62) 100%), radial-gradient(135% 85% at 50% -12%, #163a50 0%, #0b1d2c 32%, #06090f 68%, #05070c 100%), radial-gradient(58% 48% at 16% 6%, rgba(44,84,120,.34), transparent 62%), radial-gradient(52% 44% at 88% 20%, rgba(74,46,104,.26), transparent 60%); } .bg::before { content: ''; position: absolute; left: -4%; right: -4%; top: 40%; height: 230px; background-image: url("data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='1600' height='230' preserveAspectRatio='none'><path d='M0,115 H300 l15,-54 l14,108 l13,-82 l12,50 H560 l38,-13 38,13 H930 l17,-62 l16,124 l15,-76 H1240 l30,-10 30,10 H1600' fill='none' stroke='%237f93b4' stroke-width='1.3' stroke-opacity='0.4'/></svg>"); background-size: 100% 230px; background-repeat: no-repeat; opacity: .42; } .bg::after { content: ''; position: absolute; top: 27%; left: 0; height: 300px; width: 34%; background: linear-gradient(90deg, transparent, color-mix(in oklab, var(--accent) 30%, transparent) 42%, color-mix(in oklab, var(--accent) 92%, transparent) 50%, color-mix(in oklab, var(--accent) 30%, transparent) 58%, transparent); filter: blur(6px); mix-blend-mode: plus-lighter; opacity: .72; animation: sweep 12s cubic-bezier(.45,0,.55,1) infinite; } .seam { display: none; }"""
  val darkExtra =
    if clothing then ""
    else """
  .headline { animation: hglow 9s ease-in-out infinite; }
  .buy { animation: pulsebuy 6.5s ease-in-out infinite; }
  .buy:hover, .buy:focus-visible { animation: none; box-shadow: 0 0 42px 3px color-mix(in oklab, var(--accent) 72%, transparent); transform: translateY(-1px); }
  .offer:hover, .offer:focus-visible { background: linear-gradient(90deg, color-mix(in oklab, var(--accent) 9%, transparent), transparent 72%); transform: translateX(4px); box-shadow: inset 2px 0 0 var(--accent), 0 0 28px -8px color-mix(in oklab, var(--accent) 60%, transparent); }
  .kicker { font-family: var(--mono); font-size: 11.5px; letter-spacing: .34em; text-transform: uppercase; color: var(--accent); opacity: .72; margin: 0 0 20px; display: flex; align-items: center; gap: 11px; }
  .kicker::before { content: ''; width: 34px; height: 1px; background: linear-gradient(90deg, var(--accent), transparent); opacity: .8; }
  @keyframes hglow { 0%, 100% { text-shadow: 0 0 26px color-mix(in oklab, var(--accent) 16%, transparent), 0 0 2px rgba(234,241,251,.18); } 50% { text-shadow: 0 0 42px color-mix(in oklab, var(--accent) 30%, transparent), 0 0 2px rgba(234,241,251,.3); } }
  @keyframes pulsebuy { 0%, 100% { box-shadow: 0 0 0 0 rgba(0,0,0,0), 0 10px 32px rgba(0,0,0,.45); } 50% { box-shadow: 0 0 30px 1px color-mix(in oklab, var(--accent) 42%, transparent), 0 10px 32px rgba(0,0,0,.45); } }
  @keyframes sweep { 0% { transform: translateX(-130%); opacity: 0; } 9% { opacity: .72; } 90% { opacity: .72; } 100% { transform: translateX(250%); opacity: 0; } }"""
  ":root {\n  --accent: " + accent + ";\n  " + themeRoot + """
  --serif: ui-serif, "Iowan Old Style", "Palatino Linotype", Palatino, Georgia, serif;
  --mono: ui-monospace, "SF Mono", Menlo, Consolas, monospace;
  --sans: system-ui, -apple-system, "Segoe UI", Roboto, sans-serif;
}
* { box-sizing: border-box; }
body { margin: 0; color: var(--ink); font-family: var(--sans); font-size: 17px; line-height: 1.6;
  background: var(--void); min-height: 100vh; overflow-x: hidden; }
""" + themeBg + """
.glow { position: fixed; inset: 0; z-index: 0; pointer-events: none;
  background: radial-gradient(44% 30% at 50% 13%, color-mix(in oklab, var(--accent) 66%, transparent), transparent 70%);
  opacity: .13; filter: blur(58px); animation: breathe 13s ease-in-out infinite; }
@keyframes breathe { 0%, 100% { opacity: .1; } 50% { opacity: .18; } }
.wrap { position: relative; z-index: 1; max-width: 760px; margin: 0 auto; padding: 22px 22px 64px; }
.top { display: flex; justify-content: space-between; align-items: baseline;
  font-family: var(--mono); font-size: 12.5px; letter-spacing: .14em; text-transform: uppercase; }
.mark { color: var(--ink); text-decoration: none; }
.lang { display: inline-flex; gap: 2px; }
.lang button { font-family: var(--mono); font-size: 11px; letter-spacing: .08em; background: none;
  border: 0; color: var(--mist); padding: 4px 7px; cursor: pointer; border-radius: 6px; }
.lang button.on { color: var(--ink); background: rgba(142,162,190,.14); }
.lang button:hover { color: var(--ink); }
.hero { padding: 17vh 0 12vh; position: relative; }
.headline { font-family: var(--serif); font-weight: 500; font-size: clamp(32px, 7.6vw, 60px);
  line-height: 1.06; letter-spacing: -.01em; margin: 0; max-width: 18ch; text-shadow: var(--hsh); }
.lead { color: var(--mist); font-size: 18px; margin: 22px 0 30px; max-width: 46ch; }
.buy { display: inline-block; font-family: var(--mono); font-size: 14px; letter-spacing: .14em;
  text-transform: uppercase; text-decoration: none; color: #0a0d13; background: var(--accent);
  border: 0; cursor: pointer; border-radius: 999px; padding: 15px 36px; font-weight: 600;
  transition: box-shadow .3s ease, transform .2s ease; }
.buy:hover, .buy:focus-visible { box-shadow: 0 0 32px 2px var(--accent); transform: translateY(-1px); outline: none; }
.label { font-family: var(--mono); font-size: 13px; letter-spacing: .28em; text-transform: uppercase;
  color: var(--accent-text); margin: 0 0 4px; }
.offers { margin-top: 9vh; }
.offer { display: flex; align-items: center; justify-content: space-between; gap: 18px;
  padding: 20px 12px 20px 14px; border-top: 1px solid var(--line); text-decoration: none; color: inherit;
  border-radius: 12px; transition: background .25s ease, transform .25s ease, box-shadow .25s ease; }
.offers .offer:last-child { border-bottom: 1px solid var(--line); }
.offer-name { font-family: var(--serif); font-size: 20px; color: var(--ink); display: block; }
.offer-desc { color: var(--mist); font-size: 15px; display: block; margin-top: 3px; }
.offer-price { font-family: var(--mono); font-size: 14px; color: var(--ink); white-space: nowrap; }
.offer-go { font-family: var(--mono); font-size: 18px; color: var(--accent-text); opacity: .6;
  transition: opacity .25s ease, transform .25s ease; }
.offer:hover, .offer:focus-visible { background: linear-gradient(90deg, rgba(142,162,190,.07), transparent);
  transform: translateX(3px); box-shadow: inset 2px 0 0 var(--accent); outline: none; }
.offer:hover .offer-go, .offer:focus-visible .offer-go { opacity: 1; transform: translateX(4px); }
.contact { margin-top: 9vh; padding-top: 24px; border-top: 1px solid var(--line); }
.contact .body { color: var(--mist); font-size: 17px; margin: 6px 0 16px; }
.cta { display: inline-block; font-family: var(--mono); font-size: 13px; letter-spacing: .1em;
  text-transform: uppercase; text-decoration: none; color: var(--ink); background: transparent;
  border: 1px solid var(--accent); border-radius: 999px; padding: 12px 22px; cursor: pointer;
  transition: box-shadow .25s ease, background .25s ease; }
.cta:hover, .cta:focus-visible { background: rgba(142,162,190,.10); box-shadow: 0 0 22px 0 var(--accent); outline: none; }
.sla { font: 500 14px var(--mono); letter-spacing: .02em; color: var(--mist); margin: 14px 0 0; }
.sla::before { content: "\2022"; color: var(--accent-text); margin-right: 8px; }
.foot { font-family: var(--mono); font-size: 13px; letter-spacing: .12em; color: var(--mist);
  margin-top: 8vh; text-transform: uppercase; }
a:focus-visible { outline: 2px solid var(--accent); outline-offset: 4px; }
/* the intake sheet: the modal is CLOSED until a class says otherwise —
   the page carries it, the script opens it */
.modal { position: fixed; inset: 0; z-index: 50; display: none; align-items: flex-end; justify-content: center;
  background: var(--scrim); backdrop-filter: blur(7px); -webkit-backdrop-filter: blur(7px); }
.modal.open { display: flex; }
.sheet { position: relative; z-index: 1; width: 100%; max-width: 540px; max-height: 92vh; overflow-y: auto;
  background: var(--surface); border: 1px solid var(--line); border-radius: 22px 22px 0 0;
  padding: 30px 22px 34px; box-shadow: 0 -14px 70px rgba(0,0,0,.45); }
@media (min-width: 600px) { .modal { align-items: center; } .sheet { border-radius: 22px; } }
.sheet h2 { font-family: var(--serif); font-weight: 500; font-size: 27px; margin: 0 0 4px; color: var(--ink); }
.sheet .sub { color: var(--mist); font-size: 14.5px; margin: 0 0 20px; }
.x { position: absolute; top: 14px; right: 16px; background: none; border: 0; color: var(--mist);
  font-size: 26px; line-height: 1; cursor: pointer; }
.fld { display: block; margin: 0 0 15px; }
.fld > span { display: block; font-family: var(--mono); font-size: 10.5px; letter-spacing: .12em;
  text-transform: uppercase; color: var(--mist); margin: 0 0 6px; }
.fld input, .fld textarea { width: 100%; box-sizing: border-box; font-family: var(--sans); font-size: 16px;
  background: var(--field); color: var(--ink); border: 1px solid var(--line); border-radius: 12px; padding: 12px 14px; }
.fld textarea { min-height: 82px; resize: vertical; }
.fld input:focus, .fld textarea:focus { outline: none; border-color: var(--accent); }
.sheet .buy { width: 100%; text-align: center; }
/* a Schema-driven form (the offer screen): `Form.of` gives labels,
   inputs, a Select per sum and a checkbox per flag, so the theme
   dresses THOSE rather than a hand-written shape */
.formcard { background: var(--surface); border: 1px solid var(--line); border-radius: 18px;
  padding: 24px 20px 28px; margin: 0 0 9vh; }
.formcard form { display: block; }
.formcard label { display: block; margin: 0 0 15px; }
.formcard label > span { display: block; font-family: var(--mono); font-size: 10.5px;
  letter-spacing: .12em; text-transform: uppercase; color: var(--mist); margin: 0 0 6px; }
.formcard input[type=text], .formcard input:not([type]), .formcard textarea, .formcard select {
  width: 100%; box-sizing: border-box; font-family: var(--sans); font-size: 16px;
  background: var(--field); color: var(--ink); border: 1px solid var(--line);
  border-radius: 12px; padding: 12px 14px; }
.formcard select { appearance: none; -webkit-appearance: none; cursor: pointer; }
.formcard input:focus, .formcard textarea:focus, .formcard select:focus {
  outline: none; border-color: var(--accent); }
.formcard label:has(input[type=checkbox]) { display: flex; gap: 9px; align-items: flex-start;
  font-size: 13px; color: var(--mist); }
.formcard label:has(input[type=checkbox]) > span { display: inline; text-transform: none;
  font-family: var(--sans); font-size: 13px; letter-spacing: 0; margin: 0; }
.formcard input[type=checkbox] { width: auto; margin-top: 3px; }
.formcard button[type=submit] { width: 100%; font-family: var(--mono); font-size: 14px;
  letter-spacing: .14em; text-transform: uppercase; color: #0a0d13; background: var(--accent);
  border: 0; border-radius: 999px; padding: 16px; font-weight: 600; cursor: pointer; }
.formcard .okay-error { color: var(--accent-text); font-size: 13px; margin: -10px 0 14px; }
.done { text-align: center; padding: 26px 8px 8px; }
.done .big { font-family: var(--serif); font-size: 27px; color: var(--ink); margin-bottom: 8px; }
.done p { color: var(--mist); margin: 0; font-family: var(--mono); }
""" + darkExtra
```
