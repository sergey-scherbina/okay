[Shop](/lib/domain.md)

[serviceCard](/lib/cards.md)

[storefrontStyle](/lib/theme.md)

[shop](/lib/content.md)

[langs](/lib/i18n.md)

```scala declare
import okay.script.api.Inline

def tr(pl: String, en: String, uk: String, ru: String): String =
  Inline.attrs("pl" -> pl, "en" -> en, "uk" -> uk, "ru" -> ru)

def say(pl: String, en: String, uk: String, ru: String): String =
  Inline.text(langs, "pl" -> pl, "en" -> en, "uk" -> uk, "ru" -> ru)
```
<!doctype html><html lang="${langs.default}"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${shop.title}</title>
<style>${storefrontStyle(shop.clothing, shop.accent)}</style>
</head><body>
<div class="bg"><i class="seam s1"></i><i class="seam s2"></i><i class="seam s3"></i></div>
<div class="glow"></div>
<div class="wrap">
  <div class="top"><span class="mark">${shop.title}</span>${Inline.switcher(langs)}</div>
  <section class="hero">
    <h1 class="headline"${tr("Przeróbki odzieży kurierem", "Alterations, by courier", "Переробки одягу кур'єром", "Переделка одежды курьером")}>${say("Przeróbki odzieży kurierem", "Alterations, by courier", "Переробки одягу кур'єром", "Переделка одежды курьером")}</h1>
    <p class="lead"${tr("Albo wybierz gotową ofertę poniżej.", "Or pick a ready-made offer below.", "Або оберіть готову пропозицію нижче.", "Или выберите готовый оффер ниже.")}>${say("Albo wybierz gotową ofertę poniżej.", "Or pick a ready-made offer below.", "Або оберіть готову пропозицію нижче.", "Или выберите готовый оффер ниже.")}</p>
    <button class="buy" id="buy" onclick="openModal()"${tr("Kup", "Buy", "Купити", "Купить")}>${say("Kup", "Buy", "Купити", "Купить")}</button>
    <p class="sla"${tr("Odpowiadam w ciągu dnia roboczego.", "I reply within a business day.", "Відповідаю протягом робочого дня.", "Отвечаю в течение рабочего дня.")}>${say("Odpowiadam w ciągu dnia roboczego.", "I reply within a business day.", "Відповідаю протягом робочого дня.", "Отвечаю в течение рабочего дня.")}</p>
  </section>
  <section class="offers">
    <p class="label"${tr("Gotowe oferty", "Ready-made offers", "Готові пропозиції", "Готовые офферы")}>${say("Gotowe oferty", "Ready-made offers", "Готові пропозиції", "Готовые офферы")}</p>
    ${shop.services.map(sv => serviceCard(sv, shop.slug)).mkString}
  </section>
  <section class="contact" id="contact">
    <p class="label"${tr("Kontakt", "Contact", "Звʼязатися", "Связаться")}>${say("Kontakt", "Contact", "Звʼязатися", "Связаться")}</p>
    <p class="body"${tr("Napisz — wycenię i podam termin.", "Write — I will quote and give a date.", "Напишіть — оціню і назву термін.", "Напишите — оценю и назову срок.")}>${say("Napisz — wycenię i podam termin.", "Write — I will quote and give a date.", "Напишіть — оціню і назву термін.", "Напишите — оценю и назову срок.")}</p>
    <button class="cta" onclick="openModal()"${tr("Napisz →", "Get in touch →", "Написати →", "Написать →")}>${say("Napisz →", "Get in touch →", "Написати →", "Написать →")}</button>
  </section>
  <p class="foot">${shop.title}</p>
</div>
<div class="modal" id="buyModal">
  <div class="sheet">
    <button class="x" onclick="closeModal()" aria-label="close">×</button>
    <div id="buyForm">
      <h2${tr("Powiedz, czego potrzebujesz", "Tell me what you need", "Скажіть, що потрібно", "Расскажите, что нужно")}>${say("Powiedz, czego potrzebujesz", "Tell me what you need", "Скажіть, що потрібно", "Расскажите, что нужно")}</h2>
      <form onsubmit="return sendReq(this)">
        <label class="fld"><span${tr("Twoje imię", "Your name", "Ваше ім'я", "Ваше имя")}>${say("Twoje imię", "Your name", "Ваше ім'я", "Ваше имя")}</span>
          <input name="name" id="f-name" required${Inline.placeholder("pl" -> "Anna", "en" -> "Anna", "uk" -> "Анна", "ru" -> "Анна")}></label>
        <label class="fld"><span${tr("Kontakt", "Contact", "Контакт", "Контакт")}>${say("Kontakt", "Contact", "Контакт", "Контакт")}</span>
          <input name="contact" id="f-contact" required${Inline.placeholder("pl" -> "e-mail albo telefon", "en" -> "email or phone", "uk" -> "пошта або телефон", "ru" -> "почта или телефон")}></label>
        <label class="fld"><span${tr("Co trzeba zrobić", "What needs doing", "Що треба зробити", "Что нужно сделать")}>${say("Co trzeba zrobić", "What needs doing", "Що треба зробити", "Что нужно сделать")}</span>
          <textarea name="what" id="f-what" rows="3" required></textarea></label>
        <button class="buy" type="submit"${tr("Wyślij", "Send", "Надіслати", "Отправить")}>${say("Wyślij", "Send", "Надіслати", "Отправить")}</button>
      </form>
    </div>
    <div id="buyDone" style="display:none">
      <h2${tr("Dziękuję — odezwę się.", "Thank you — I will be in touch.", "Дякую — зв'яжуся.", "Спасибо — свяжусь.")}>${say("Dziękuję — odezwę się.", "Thank you — I will be in touch.", "Дякую — зв'яжуся.", "Спасибо — свяжусь.")}</h2>
      <p class="sub" id="buyRef"></p>
    </div>
  </div>
</div>
<script>
function openModal(){ document.getElementById('buyModal').classList.add('open'); }
function closeModal(){ document.getElementById('buyModal').classList.remove('open'); }
function sendReq(f){
  var d = new FormData(f), b = new URLSearchParams();
  d.forEach(function(v, k){ b.append(k, v); });
  var btn = f.querySelector('button[type=submit]'); if (btn) btn.disabled = true;
  fetch('/request', { method: 'POST', headers: { 'Content-Type': 'application/x-www-form-urlencoded' }, body: b.toString() })
    .then(function(r){ return r.json(); })
    .then(function(j){
      document.getElementById('buyForm').style.display = 'none';
      var done = document.getElementById('buyDone');
      done.style.display = 'block';
      if (j.ref) document.getElementById('buyRef').textContent = j.ref;
    })
    .catch(function(){ if (btn) btn.disabled = false; });
  return false;
}
</script>
${Inline.script(langs)}
</body></html>
