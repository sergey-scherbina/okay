package okay.r

import okay.{Writer, given}

object TestRSource:
  val m = R.module("rsources", """
    rows <- function(n, size) {
      i <- 0L
      function() {
        if (i >= n) return(NULL)
        xs <- seq.int(i, min(i + size, n) - 1L)
        i <<- i + size
        xs
      }
    }
  """)

/** a far-side SOURCE in R (foreign-one-mux): a closure answering the next
 * chunk per call and NULL at the end, read at the consumer's pace (Live) */
class TestRSource extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  test("every element, in order, from an R closure; NULL ends it") {
    val r = RSubprocess.start(TestR.rscript.get, modules = Seq(TestRSource.m))
    try
      val out = Writer.run(R.releasing(R.source[Long]("rsources::rows")(10L, 3L))).runWith(using r.handler)._1.toList
      assertEquals(out, (0L until 10L).toList)
    finally r.close()
  }
