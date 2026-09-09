package okay.script

/** The browser side of a Live page: a dependency-free patch consumer
 * speaking okay-ui's `Protocol` -- the derived tree/patch/event shapes,
 * the same `React.elem` DOM structure (so a patch path walks the
 * same `childNodes`), the same delegated-listener event mapping the
 * Scala.js `Dom` backend uses, in ~100 lines of plain JavaScript
 * served at `Live.JsPath`. Hand-written rather than linked from
 * Scala.js so that a Site needs no build step and no artifact: the
 * page IS the deployment. See specs/okay-script.md "Live pages".
 */
object LiveJs:
  val source: String =
    """(function () {
      |  // one definition: okay.ui.Protocol's derived shapes (docs/protocol/frontend.md).
      |  // a node is {"Case": {fields}}; k(u) is its case name, u[k(u)] its fields
      |  function k(u) { for (var n in u) return n; }
      |  function build(u) {
      |    var el, i, t = k(u), f = u[t];
      |    switch (t) {
      |      case "Text":
      |        el = document.createElement("span");
      |        var cls = [], st = f.style || {};
      |        if (st.bold) cls.push("okay-bold");
      |        if (st.dim) cls.push("okay-dim");
      |        if (st.tone && st.tone !== "plain") cls.push("okay-tone-" + st.tone);
      |        if (st.size && st.size !== "normal") cls.push("okay-size-" + st.size);
      |        if (cls.length) el.className = cls.join(" ");
      |        el.textContent = f.s;
      |        return el;
      |      case "Row":
      |      case "Column":
      |        el = document.createElement("div");
      |        el.className = t === "Row" ? "okay-row" : "okay-col";
      |        if (f.key) el.dataset.key = f.key;
      |        for (i = 0; i < f.children.length; i++) el.appendChild(build(f.children[i]));
      |        return el;
      |      case "Box":
      |        el = document.createElement("div");
      |        el.className = f.dir === "h" ? "okay-box okay-h" : "okay-box okay-v";
      |        if (f.gap) el.style.gap = f.gap + "ch";
      |        if (f.pad) el.style.padding = f.pad + "ch";
      |        if (f.key) el.dataset.key = f.key;
      |        if (f.weights && f.weights.length === f.children.length) el.dataset.w = f.weights.join(" ");
      |        for (i = 0; i < f.children.length; i++) el.appendChild(weighed(el, i, build(f.children[i])));
      |        return el;
      |      case "Form":
      |        // the hybrid rule: the DOM holds the fields' values; the
      |        // button keyed like the form sends them once as Submitted
      |        el = document.createElement("div");
      |        el.className = "okay-form";
      |        el.dataset.form = f.key;
      |        for (i = 0; i < f.fields.length; i++) el.appendChild(build(f.fields[i]));
      |        el.appendChild(build({ Button: { label: f.submit, key: f.key, role: "primary" } }));
      |        return el;
      |      case "Scroll":
      |        el = document.createElement("div");
      |        el.className = "okay-scroll";
      |        el.style.overflow = "auto";
      |        if (f.key) el.dataset.key = f.key;
      |        el.appendChild(build(f.child));
      |        return el;
      |      case "Image":
      |        el = document.createElement("img");
      |        el.src = f.src;
      |        el.alt = f.alt;
      |        return el;
      |      case "Button":
      |        el = document.createElement("button");
      |        if (f.key) el.dataset.key = f.key;
      |        if (f.role && f.role !== "plain") el.className = "okay-" + f.role;
      |        el.textContent = f.label;
      |        return el;
      |      case "Input":
      |        if (f.kind === "multiline") el = document.createElement("textarea");
      |        else {
      |          el = document.createElement("input");
      |          if (f.kind === "secret") el.type = "password";
      |          else if (f.kind === "number") el.type = "number";
      |        }
      |        if (f.key) el.dataset.key = f.key;
      |        if (f.live) el.dataset.live = "1";
      |        el.value = f.value;
      |        if (!f.label) return el;
      |        var lab = document.createElement("label");
      |        var sp = document.createElement("span");
      |        sp.textContent = f.label;
      |        lab.appendChild(sp);
      |        lab.appendChild(el);
      |        return lab;
      |      case "Check":
      |        el = document.createElement("input");
      |        el.type = "checkbox";
      |        if (f.key) el.dataset.key = f.key;
      |        el.checked = !!f.on;
      |        if (!f.label) return el;
      |        var lab2 = document.createElement("label");
      |        var sp2 = document.createElement("span");
      |        sp2.textContent = f.label;
      |        lab2.appendChild(el);
      |        lab2.appendChild(sp2);
      |        return lab2;
      |      case "Select":
      |        el = document.createElement("select");
      |        if (f.key) el.dataset.key = f.key;
      |        for (i = 0; i < f.options.length; i++) {
      |          var o = document.createElement("option");
      |          o.value = f.options[i];
      |          o.textContent = f.options[i];
      |          el.appendChild(o);
      |        }
      |        el.selectedIndex = f.selected;
      |        return el;
      |    }
      |    return document.createTextNode("");
      |  }
      |  function weighed(par, i, ch) {
      |    if (par.dataset && par.dataset.w) { var w = par.dataset.w.split(" "); if (w[i]) ch.style.flex = w[i]; }
      |    return ch;
      |  }
      |  function editable(n) {
      |    var tag = n.tagName.toLowerCase();
      |    if (tag === "input" || tag === "select" || tag === "textarea") return n;
      |    for (var i = 0; i < n.childNodes.length; i++) {
      |      var t = n.childNodes[i].tagName && n.childNodes[i].tagName.toLowerCase();
      |      if (t === "input" || t === "textarea") return n.childNodes[i];
      |    }
      |    return n;
      |  }
      |  window.okayLive = function (id) {
      |    var root = document.getElementById("okay-live-" + id);
      |    if (!root) return;
      |    function at(path) {
      |      var n = root.childNodes[0];
      |      for (var i = 0; i < path.length; i++) n = n.childNodes[path[i]];
      |      return n;
      |    }
      |    function apply(p) {
      |      var n, par, snap, t = k(p), f = p[t];
      |      switch (t) {
      |        case "Replace":
      |          if (f.path.length === 0) {
      |            var b = build(f.ui);
      |            if (root.childNodes.length) root.replaceChild(b, root.childNodes[0]); else root.appendChild(b);
      |          } else {
      |            par = at(f.path.slice(0, -1));
      |            par.replaceChild(weighed(par, f.path[f.path.length - 1], build(f.ui)), par.childNodes[f.path[f.path.length - 1]]);
      |          }
      |          break;
      |        case "SetText": at(f.path).textContent = f.s; break;
      |        case "SetValue": editable(at(f.path)).value = f.s; break;
      |        case "SetChecked": editable(at(f.path)).checked = f.on; break;
      |        case "SetSelected": at(f.path).selectedIndex = f.index; break;
      |        case "Remove": n = at(f.path); n.removeChild(n.childNodes[f.index]); break;
      |        case "Reorder":
      |          n = at(f.path);
      |          snap = Array.prototype.slice.call(n.childNodes);
      |          for (var i = 0; i < f.order.length; i++) n.appendChild(snap[f.order[i]]);
      |          break;
      |        case "Insert":
      |          n = at(f.path);
      |          n.insertBefore(weighed(n, f.index, build(f.ui)), f.index < n.childNodes.length ? n.childNodes[f.index] : null);
      |          break;
      |      }
      |    }
      |    var proto = location.protocol === "https:" ? "wss://" : "ws://";
      |    var sep = location.search ? "&" : "?";
      |    var ws = new WebSocket(proto + location.host + location.pathname + location.search + sep + "__live=" + encodeURIComponent(id));
      |    function send(o) { if (ws.readyState === 1) ws.send(JSON.stringify(o)); }
      |    function event(e) { send({ Event: { event: e } }); }
      |    // the hello: this client draws the layout level only
      |    ws.onopen = function () { send({ Hello: { vocab: [], version: 1 } }); };
      |    ws.onmessage = function (m) {
      |      var j = JSON.parse(m.data), t = k(j);
      |      if (t === "Tree") { while (root.firstChild) root.removeChild(root.firstChild); root.appendChild(build(j.Tree.ui)); }
      |      else if (t === "Patch") apply(j.Patch.patch);
      |    };
      |    function keyed(el) {
      |      while (el && el !== root) { if (el.dataset && el.dataset.key) return el; el = el.parentNode; }
      |      return null;
      |    }
      |    function formOf(el) {
      |      while (el && el !== root) { if (el.dataset && el.dataset.form) return el; el = el.parentNode; }
      |      return null;
      |    }
      |    function edits(form) {
      |      var out = [], els = form.querySelectorAll("[data-key]");
      |      for (var i = 0; i < els.length; i++) {
      |        var e = els[i], tag = e.tagName;
      |        if (tag === "INPUT" && e.type === "checkbox") out.push({ Toggled: { key: e.dataset.key, on: e.checked } });
      |        else if (tag === "INPUT" || tag === "TEXTAREA") out.push({ Edited: { key: e.dataset.key, value: e.value } });
      |        else if (tag === "SELECT") out.push({ Chosen: { key: e.dataset.key, index: e.selectedIndex } });
      |      }
      |      return out;
      |    }
      |    root.addEventListener("click", function (ev) {
      |      var el = keyed(ev.target);
      |      if (!el || el.tagName !== "BUTTON") return;
      |      var form = formOf(el);
      |      if (form && form.dataset.form === el.dataset.key) event({ Submitted: { key: el.dataset.key, edits: edits(form) } });
      |      else event({ Pressed: { key: el.dataset.key } });
      |    });
      |    // inside a form the DOM keeps the value; only a live input speaks
      |    root.addEventListener("input", function (ev) {
      |      var el = keyed(ev.target);
      |      if (!el || !(el.tagName === "TEXTAREA" || (el.tagName === "INPUT" && el.type !== "checkbox"))) return;
      |      if (formOf(el) && !el.dataset.live) return;
      |      event({ Edited: { key: el.dataset.key, value: el.value } });
      |    });
      |    root.addEventListener("change", function (ev) {
      |      var el = keyed(ev.target);
      |      if (!el || formOf(el)) return;
      |      if (el.tagName === "INPUT" && el.type === "checkbox") event({ Toggled: { key: el.dataset.key, on: el.checked } });
      |      else if (el.tagName === "SELECT") event({ Chosen: { key: el.dataset.key, index: el.selectedIndex } });
      |    });
      |    window.addEventListener("beforeunload", function () { event({ Closed: {} }); });
      |  };
      |})();
      |""".stripMargin
