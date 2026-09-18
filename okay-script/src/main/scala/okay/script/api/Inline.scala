package okay.script.api

/**
 * INLINE i18n: every language on the element, the client picks
 * (specs/site-framework.md stage 3).
 *
 * okay-script's own i18n is per REQUEST — a `page.uk.md` variant or
 * `t(key)` — and answers one language per response, which is what an
 * indexable page and a scriptless browser want. The sites this arc is
 * measured against answer a different question: a visitor switching
 * language with no round trip, on a page a CDN may have cached. They
 * do it by carrying every language on the element (`data-pl`,
 * `data-uk`, …) and swapping `textContent` on load.
 *
 * Both are here now and a page chooses. They COMPOSE: the element's
 * own content is still rendered server-side in the request's
 * language, so the page is right before any script runs and right
 * without one — the attributes only let the visitor change their
 * mind.
 *
 * The text must be TEXT: a swap sets `textContent`, so markup inside
 * a translated element would be destroyed by the first switch. Said
 * here because the sites learnt it the hard way and wrote it in a
 * comment.
 */
object Inline:

  /**
   * The languages a page offers, and the one its server-rendered
   * content is in.
   *
   * `default` is a METHOD, not a field, and that is the whole design:
   * a page's declare block holds `val langs = Langs.of(...)`, and a
   * `val` there is evaluated once per COMPILE, not per request
   * (specs/okay-script.md "Declarations"). A `default` frozen at
   * construction froze the first visitor's language for every visitor
   * after them — measured, by a `?lang=en` request that answered
   * Ukrainian. Reading `Lang.current` at use time makes the value
   * request-independent and keeps the request-dependent part a
   * function, which is where it belongs.
   *
   * `pinned` overrides that, for a page that renders one language on
   * purpose.
   */
  final case class Langs(all: Vector[String], pinned: Option[String] = None):
    require(all.nonEmpty, "a page with no language offers nothing")
    def default: String =
      pinned.orElse(Some(Lang.current)).filter(all.contains).getOrElse(all.head)
    def other: Vector[String] = all.filterNot(_ == default)

  object Langs:
    def of(all: String*): Langs = Langs(all.toVector)

  /** the attributes for one translated TEXT: `by` maps a language to
   * its text. A language a page offers but this text lacks simply has
   * no attribute, and the swap leaves the element alone — a partial
   * translation degrades to the rendered text rather than to blank. */
  def attrs(by: (String, String)*): String =
    by.filter((_, v) => v.nonEmpty)
      .map((l, v) => s""" data-$l="${escape(v)}"""").mkString

  /** the same for an input's placeholder, which is an attribute and
   * not a text node */
  def placeholder(by: (String, String)*): String =
    by.filter((_, v) => v.nonEmpty)
      .map((l, v) => s""" data-ph-$l="${escape(v)}"""").mkString

  /** what this element says NOW: the default language's text, falling
   * back to the first one given — the content the server renders and
   * a scriptless browser keeps */
  def text(langs: Langs, by: (String, String)*): String =
    by.toMap.get(langs.default).filter(_.nonEmpty)
      .orElse(by.collectFirst { case (_, v) if v.nonEmpty => v })
      .getOrElse("")

  /** one translated element, whole: the tag, its own attributes, the
   * languages and the text. The common case in one call, so a page
   * does not repeat `attrs` and `text` and risk them disagreeing. */
  def span(langs: Langs, by: (String, String)*): String =
    element("span", "", langs, by*)

  def element(tag: String, ownAttrs: String, langs: Langs, by: (String, String)*): String =
    val known = by.filter((l, _) => langs.all.contains(l))
    s"<$tag$ownAttrs${attrs(known*)}>${escape(text(langs, known*))}</$tag>"

  /** the switcher: one button per language, the current one marked.
   * `setLang` is the script's own, so a page that draws its own
   * switcher can call it too. */
  def switcher(langs: Langs, className: String = "lang"): String =
    langs.all.map { l =>
      val on = if l == langs.default then """ class="on"""" else ""
      s"""<button type="button" data-l="$l"$on onclick="setLang('$l')">${escape(l.toUpperCase)}</button>"""
    }.mkString(s"""<div class="$className">""", "", "</div>")

  /** the client: ~20 lines, dependency-free, that pick a language and
   * apply it. The choice is the cookie, then `localStorage`, then the
   * browser's own, then the page's default — cookie first so a
   * server that also renders per request agrees with the client. */
  def script(langs: Langs): String =
    val sup = langs.all.map(l => s"'$l'").mkString("[", ",", "]")
    s"""<script>(function(){
       |  var sup=$sup, dflt='${langs.default}';
       |  function ck(n){ var m=document.cookie.match('(^|; )'+n+'=([^;]*)'); return m?decodeURIComponent(m[2]):''; }
       |  function pick(){ var c=ck('${Lang.Cookie}'); if(c&&sup.indexOf(c)>=0)return c;
       |    var s=null; try{ s=localStorage.getItem('lang'); }catch(e){}
       |    if(s&&sup.indexOf(s)>=0)return s;
       |    var b=(navigator.language||dflt).slice(0,2).toLowerCase();
       |    return sup.indexOf(b)>=0?b:dflt; }
       |  function apply(l){ document.documentElement.lang=l;
       |    var es=document.querySelectorAll('[data-'+l+']');
       |    for(var i=0;i<es.length;i++){ var v=es[i].getAttribute('data-'+l); if(v)es[i].textContent=v; }
       |    var ps=document.querySelectorAll('[data-ph-'+l+']');
       |    for(var p=0;p<ps.length;p++){ var pv=ps[p].getAttribute('data-ph-'+l); if(pv)ps[p].setAttribute('placeholder',pv); }
       |    var ts=document.querySelectorAll('[data-l]');
       |    for(var j=0;j<ts.length;j++){ ts[j].className=(ts[j].getAttribute('data-l')===l)?'on':''; } }
       |  window.getLang=pick;
       |  window.setLang=function(l){ try{ localStorage.setItem('lang',l); }catch(e){}
       |    document.cookie='${Lang.Cookie}='+encodeURIComponent(l)+'; path=/; max-age=31536000; SameSite=Lax';
       |    apply(l); };
       |  apply(pick());
       |})();</script>""".stripMargin

  private def escape(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")
