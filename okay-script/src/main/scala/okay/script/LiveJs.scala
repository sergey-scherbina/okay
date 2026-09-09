package okay.script

/** The browser side of a Live page: a dependency-free patch consumer
 * speaking okay-ui's `WireJson` -- the same tree/patch/event shapes,
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
      |  function build(u) {
      |    var el, i;
      |    switch (u.t) {
      |      case "text":
      |        el = document.createElement("span");
      |        var cls = [];
      |        if (u.bold) cls.push("okay-bold");
      |        if (u.dim) cls.push("okay-dim");
      |        if (u.tone) cls.push("okay-tone-" + u.tone);
      |        if (u.size) cls.push("okay-size-" + u.size);
      |        if (cls.length) el.className = cls.join(" ");
      |        el.textContent = u.s;
      |        return el;
      |      case "row":
      |      case "col":
      |        el = document.createElement("div");
      |        el.className = u.t === "row" ? "okay-row" : "okay-col";
      |        if (u.k) el.dataset.key = u.k;
      |        for (i = 0; i < u.c.length; i++) el.appendChild(build(u.c[i]));
      |        return el;
      |      case "box":
      |        el = document.createElement("div");
      |        el.className = u.dir === "h" ? "okay-box okay-h" : "okay-box okay-v";
      |        if (u.gap) el.style.gap = u.gap + "ch";
      |        if (u.pad) el.style.padding = u.pad + "ch";
      |        if (u.k) el.dataset.key = u.k;
      |        if (u.w && u.w.length === u.c.length) el.dataset.w = u.w.join(" ");
      |        for (i = 0; i < u.c.length; i++) el.appendChild(weighed(el, i, build(u.c[i])));
      |        return el;
      |      case "scroll":
      |        el = document.createElement("div");
      |        el.className = "okay-scroll";
      |        el.style.overflow = "auto";
      |        if (u.k) el.dataset.key = u.k;
      |        el.appendChild(build(u.c[0]));
      |        return el;
      |      case "image":
      |        el = document.createElement("img");
      |        el.src = u.src;
      |        el.alt = u.alt;
      |        return el;
      |      case "button":
      |        el = document.createElement("button");
      |        if (u.k) el.dataset.key = u.k;
      |        if (u.role && u.role !== "plain") el.className = "okay-" + u.role;
      |        el.textContent = u.label;
      |        return el;
      |      case "input":
      |        if (u.kind === "multiline") el = document.createElement("textarea");
      |        else {
      |          el = document.createElement("input");
      |          if (u.kind === "secret") el.type = "password";
      |          else if (u.kind === "number") el.type = "number";
      |        }
      |        if (u.k) el.dataset.key = u.k;
      |        el.value = u.value;
      |        if (!u.label) return el;
      |        var lab = document.createElement("label");
      |        var sp = document.createElement("span");
      |        sp.textContent = u.label;
      |        lab.appendChild(sp);
      |        lab.appendChild(el);
      |        return lab;
      |      case "check":
      |        el = document.createElement("input");
      |        el.type = "checkbox";
      |        if (u.k) el.dataset.key = u.k;
      |        el.checked = !!u.on;
      |        if (!u.label) return el;
      |        var lab2 = document.createElement("label");
      |        var sp2 = document.createElement("span");
      |        sp2.textContent = u.label;
      |        lab2.appendChild(el);
      |        lab2.appendChild(sp2);
      |        return lab2;
      |      case "select":
      |        el = document.createElement("select");
      |        if (u.k) el.dataset.key = u.k;
      |        for (i = 0; i < u.options.length; i++) {
      |          var o = document.createElement("option");
      |          o.value = u.options[i];
      |          o.textContent = u.options[i];
      |          el.appendChild(o);
      |        }
      |        el.selectedIndex = u.i;
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
      |      var n, par, snap;
      |      switch (p.p) {
      |        case "replace":
      |          if (p.at.length === 0) {
      |            var b = build(p.ui);
      |            if (root.childNodes.length) root.replaceChild(b, root.childNodes[0]); else root.appendChild(b);
      |          } else {
      |            par = at(p.at.slice(0, -1));
      |            par.replaceChild(weighed(par, p.at[p.at.length - 1], build(p.ui)), par.childNodes[p.at[p.at.length - 1]]);
      |          }
      |          break;
      |        case "text": at(p.at).textContent = p.s; break;
      |        case "value": editable(at(p.at)).value = p.s; break;
      |        case "checked": editable(at(p.at)).checked = p.on; break;
      |        case "selected": at(p.at).selectedIndex = p.i; break;
      |        case "remove": n = at(p.at); n.removeChild(n.childNodes[p.i]); break;
      |        case "reorder":
      |          n = at(p.at);
      |          snap = Array.prototype.slice.call(n.childNodes);
      |          for (var i = 0; i < p.order.length; i++) n.appendChild(snap[p.order[i]]);
      |          break;
      |        case "insert":
      |          n = at(p.at);
      |          n.insertBefore(weighed(n, p.i, build(p.ui)), p.i < n.childNodes.length ? n.childNodes[p.i] : null);
      |          break;
      |      }
      |    }
      |    var proto = location.protocol === "https:" ? "wss://" : "ws://";
      |    var sep = location.search ? "&" : "?";
      |    var ws = new WebSocket(proto + location.host + location.pathname + location.search + sep + "__live=" + encodeURIComponent(id));
      |    function send(o) { if (ws.readyState === 1) ws.send(JSON.stringify(o)); }
      |    ws.onmessage = function (m) {
      |      var j = JSON.parse(m.data);
      |      if (j.t) { while (root.firstChild) root.removeChild(root.firstChild); root.appendChild(build(j)); }
      |      else if (j.p) apply(j);
      |    };
      |    function keyed(el) {
      |      while (el && el !== root) { if (el.dataset && el.dataset.key) return el; el = el.parentNode; }
      |      return null;
      |    }
      |    root.addEventListener("click", function (ev) {
      |      var el = keyed(ev.target);
      |      if (el && el.tagName === "BUTTON") send({ e: "press", k: el.dataset.key });
      |    });
      |    root.addEventListener("input", function (ev) {
      |      var el = keyed(ev.target);
      |      if (el && el.tagName === "INPUT" && el.type !== "checkbox") send({ e: "edit", k: el.dataset.key, v: el.value });
      |    });
      |    root.addEventListener("change", function (ev) {
      |      var el = keyed(ev.target);
      |      if (!el) return;
      |      if (el.tagName === "INPUT" && el.type === "checkbox") send({ e: "toggle", k: el.dataset.key, on: el.checked });
      |      else if (el.tagName === "SELECT") send({ e: "choose", k: el.dataset.key, i: el.selectedIndex });
      |    });
      |    window.addEventListener("beforeunload", function () { send({ e: "closed" }); });
      |  };
      |})();
      |""".stripMargin
