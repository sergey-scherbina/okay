package okay.clojure

import okay.codec.{Edn, Schema}

/**
 * okay's EDN against Clojure's own reader and printer (edn-codec): the
 * text okay writes is what `clojure.edn/read-string` reads — keyword
 * keys, an exact Long, a `N` BigInt, a character, a namespaced tag — and
 * what Clojure's `pr-str` prints (commas and all) is what okay reads
 * back into a typed value.
 */
class TestEdnWithClojure extends munit.FunSuite {

  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  final case class Doc(name: String, count: Long, big: BigInt, initial: Char, sizes: Vector[Int], shape: Shape)
    derives Schema

  val doc = Doc("okay", Long.MaxValue, BigInt("123456789012345678901234567890"), 'o', Vector(1, 2, 3), Shape.Rect(2.0, 3.5))

  def clj(src: String): AnyRef = Clj.eval(src).fold(e => fail(e), identity)

  test("okay writes EDN that clojure.edn reads: keywords, an exact Long, N, \\c, a tag") {
    clj("(require 'clojure.edn)")
    clj("(defn okay-edn-read [s] (clojure.edn/read-string {:default tagged-literal} s))")
    val m = Clj.fn("user", "okay-edn-read").fold(e => fail(e), identity).invoke(Edn.write(doc))
    def get(k: String): String = clj(s"(fn [m] (pr-str (get m $k)))") match
      case f: clojure.lang.IFn => f.invoke(m).toString
      case other => fail(s"not a function: $other")
    assertEquals(get(":name"), "\"okay\"")
    assertEquals(get(":count"), "9223372036854775807")
    assertEquals(get(":big"), "123456789012345678901234567890N")
    assertEquals(get(":initial"), "\\o")
    assertEquals(get(":sizes"), "[1 2 3]")
    assertEquals(get(":shape"), "#Shape/Rect {:w 2.0, :h 3.5}")
  }

  test("Clojure's pr-str is read by okay's Edn into a typed value") {
    val printed = clj("""(pr-str {:name "okay" :count 9223372036854775807 :big 123456789012345678901234567890N
                                   :initial \o :sizes [1 2 3] :shape (tagged-literal 'Shape/Rect {:w 2.0 :h 3.5})})""")
    assertEquals(Edn.read[Doc](printed.toString), Right(doc))
  }
}
