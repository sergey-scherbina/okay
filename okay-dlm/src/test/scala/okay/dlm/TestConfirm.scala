package okay.dlm

import munit.FunSuite

class TestConfirm extends FunSuite:
  import Confirm.Answer

  val words = Confirm.Words(
    yes = Confirm.english.yes ++ Set("да", "ага", "конечно", "давай", "ок"),
    no = Confirm.english.no ++ Set("нет", "неа", "не"),
    polite = Confirm.english.polite ++ Set("пожалуйста", "спасибо"),
    steer = Confirm.english.steer ++ Set("отмена", "стоп"),
    tell = Confirm.english.tell ++ Set("что", "кто", "подробнее"),
    leadIn = Confirm.english.leadIn ++ Set("а", "ну"))
  val c = Confirm(words)

  test("a yes or no is a whole, short reply of answer and courtesy words") {
    assertEquals(c.of("да"), Some(true))
    assertEquals(c.of("Yes please"), Some(true))
    assertEquals(c.of("нет, спасибо"), Some(false))
    assertEquals(c.of("не, лучше по-другому"), None)   // words that are neither: a fresh request
    assertEquals(c.of("да нет"), None)                  // both: nobody knows
    assertEquals(c.of(""), None)
  }

  test("a yes that LEADS a longer message hands back the rest") {
    assertEquals(c.leading("Да, только а где про Scala?"), Some((true, "только а где про Scala?")))
    assertEquals(c.leading("нет — другой город"), Some((false, "другой город")))
    assertEquals(c.leading("город Вроцлав"), None)
  }

  test("three meanings: yes, no, and tell me more — read by rule first") {
    assertEquals(c.rule("?"), Some(Answer.Tell))
    assertEquals(c.rule("а что там?"), Some(Answer.Tell))
    assertEquals(c.rule("кто"), Some(Answer.Tell))
    assertEquals(c.rule("да"), Some(Answer.Yes))
    assertEquals(c.rule("Нет, не надо"), Some(Answer.No))
    assertEquals(c.rule("расскажи мне про это подробно и без спешки"), None)
    // the head answers where the rule is silent, and its words are three
    assertEquals(c.answer("хм", _ => Some("tell")), Some(Answer.Tell))
    assertEquals(c.answer("хм", _ => Some("other")), None)
    assertEquals(c.answer("да", _ => Some("no")), Some(Answer.Yes))
  }

  test("the offer's own verb is a yes; a bare subject is not") {
    val act = Set("добавь", "добавить", "add")
    assert(c.taking("Добавь", act))
    assert(c.taking("да, добавь источник", act, about = Set("источник")))
    assert(!c.taking("источник", act, about = Set("источник")))
    assert(!c.taking("не добавляй", act))
    assert(!c.taking("добавь и покажи всё", act))
  }

  test("a control word is a reply with no answer in it") {
    assert(c.control("отмена"))
    assert(c.control("да, спасибо"))
    assert(!c.control("да, сантехник"))
    assert(!c.control(""))
  }
