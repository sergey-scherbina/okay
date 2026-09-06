package okay.intent

/**
 * Number words per language, in one place, shared by every parser that
 * counts (`Duration`, `People`): the cardinal words with their genders,
 * the tens, and the one-and-a-half words. Doubles because `anderthalb`
 * is one; a parser that wants an integer rounds.
 */
object Numbers {
  val fr: Map[String, Double] = Map("un" -> 1.0, "une" -> 1.0, "deux" -> 2.0, "trois" -> 3.0, "quatre" -> 4.0, "cinq" -> 5.0, "six" -> 6.0,
        "sept" -> 7.0, "huit" -> 8.0, "neuf" -> 9.0, "dix" -> 10.0, "onze" -> 11.0, "douze" -> 12.0, "quinze" -> 15.0,
        "vingt" -> 20.0, "trente" -> 30.0, "quarante" -> 40.0, "quarante-cinq" -> 45.0, "cinquante" -> 50.0,
        "soixante" -> 60.0, "quatre-vingt-dix" -> 90.0, "demie" -> 0.5)

  val de: Map[String, Double] = Map("ein" -> 1.0, "eine" -> 1.0, "eins" -> 1.0, "einer" -> 1.0, "zwei" -> 2.0, "drei" -> 3.0, "vier" -> 4.0,
        "fünf" -> 5.0, "fuenf" -> 5.0, "sechs" -> 6.0, "sieben" -> 7.0, "acht" -> 8.0, "neun" -> 9.0, "zehn" -> 10.0,
        "elf" -> 11.0, "zwölf" -> 12.0, "zwoelf" -> 12.0, "fünfzehn" -> 15.0, "zwanzig" -> 20.0, "dreißig" -> 30.0,
        "dreissig" -> 30.0, "vierzig" -> 40.0, "fünfundvierzig" -> 45.0, "fünfzig" -> 50.0, "sechzig" -> 60.0, "neunzig" -> 90.0,
        "anderthalb" -> 1.5, "eineinhalb" -> 1.5, "zweieinhalb" -> 2.5, "dreieinhalb" -> 3.5)

  val es: Map[String, Double] = Map("un" -> 1.0, "una" -> 1.0, "uno" -> 1.0, "dos" -> 2.0, "tres" -> 3.0, "cuatro" -> 4.0, "cinco" -> 5.0,
        "seis" -> 6.0, "siete" -> 7.0, "ocho" -> 8.0, "nueve" -> 9.0, "diez" -> 10.0, "once" -> 11.0, "doce" -> 12.0,
        "quince" -> 15.0, "veinte" -> 20.0, "treinta" -> 30.0, "cuarenta" -> 40.0, "cincuenta" -> 50.0, "sesenta" -> 60.0, "noventa" -> 90.0)

  val ru: Map[String, Double] = Map("один" -> 1.0, "одна" -> 1.0, "одну" -> 1.0, "два" -> 2.0, "две" -> 2.0, "три" -> 3.0, "четыре" -> 4.0,
        "пять" -> 5.0, "шесть" -> 6.0, "семь" -> 7.0, "восемь" -> 8.0, "девять" -> 9.0, "десять" -> 10.0, "одиннадцать" -> 11.0,
        "двенадцать" -> 12.0, "пятнадцать" -> 15.0, "двадцать" -> 20.0, "тридцать" -> 30.0, "сорок" -> 40.0,
        "пятьдесят" -> 50.0, "шестьдесят" -> 60.0, "девяносто" -> 90.0, "полтора" -> 1.5)

  val uk: Map[String, Double] = Map("один" -> 1.0, "одна" -> 1.0, "одну" -> 1.0, "два" -> 2.0, "дві" -> 2.0, "три" -> 3.0, "чотири" -> 4.0,
        "п'ять" -> 5.0, "п’ять" -> 5.0, "шість" -> 6.0, "сім" -> 7.0, "вісім" -> 8.0, "дев'ять" -> 9.0, "дев’ять" -> 9.0,
        "десять" -> 10.0, "одинадцять" -> 11.0, "дванадцять" -> 12.0, "п'ятнадцять" -> 15.0, "п’ятнадцять" -> 15.0,
        "двадцять" -> 20.0, "тридцять" -> 30.0, "сорок" -> 40.0, "п'ятдесят" -> 50.0, "п’ятдесят" -> 50.0,
        "шістдесят" -> 60.0, "дев'яносто" -> 90.0, "дев’яносто" -> 90.0, "півтори" -> 1.5)

  val pl: Map[String, Double] = Map("jeden" -> 1.0, "jedna" -> 1.0, "jedną" -> 1.0, "dwa" -> 2.0, "dwie" -> 2.0, "trzy" -> 3.0, "cztery" -> 4.0,
        "pięć" -> 5.0, "sześć" -> 6.0, "siedem" -> 7.0, "osiem" -> 8.0, "dziewięć" -> 9.0, "dziesięć" -> 10.0,
        "jedenaście" -> 11.0, "dwanaście" -> 12.0, "piętnaście" -> 15.0, "dwadzieścia" -> 20.0, "trzydzieści" -> 30.0,
        "czterdzieści" -> 40.0, "pięćdziesiąt" -> 50.0, "sześćdziesiąt" -> 60.0, "dziewięćdziesiąt" -> 90.0, "półtorej" -> 1.5)

}
