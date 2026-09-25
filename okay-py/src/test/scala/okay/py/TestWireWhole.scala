package okay.py

/** a wire line is read strictly: a reply cut short is refused, never read as a smaller reply (default gate) */
class TestWireWhole extends munit.FunSuite:

  test("a whole line is read") {
    assertEquals(ForeignWorker.whole("""{"id":1,"ok":{"done":22}}""").toString.nonEmpty, true)
  }

  test("a line cut short is refused, whatever byte it lost") {
    val line = """{"id":1,"ok":{"args":[[1,2]],"k":1,"perform":"choose"}}"""
    val accepted = (1 until line.length).filter(n => scala.util.Try(ForeignWorker.whole(line.dropRight(n))).isSuccess)
    assertEquals(accepted.toVector, Vector.empty)
  }

  test("over a link, a truncated handshake refuses loudly") {
    val link = new WireLink:
      def hello(): Option[String] = Some(s"""{"shim":${ForeignWorker.ShimVersion},"python":"go"""")
      def roundTrip(line: String): Option[String] = None
      def exchange(message: Array[Byte]): Option[Array[Byte]] = None
      def close(): Unit = ()
    val e = intercept[IllegalStateException](ForeignWorker.over(link))
    assert(e.getMessage.contains("not whole"), e.getMessage)
  }
