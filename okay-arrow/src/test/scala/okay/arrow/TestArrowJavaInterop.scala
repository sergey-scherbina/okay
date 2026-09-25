package okay.arrow

import org.apache.arrow.memory.RootAllocator

/** stage 0's precondition: each side reads the other's bytes and gets
 * the same table — no number is recorded otherwise */
class TestArrowJavaInterop extends munit.FunSuite:

  test("Arrow Java reads the stream ArrowIpc writes, value for value") {
    val d = ArrowJava.data(10000)
    val alloc = RootAllocator()
    try assert(ArrowJava.same(d, ArrowJava.readToArrays(alloc, okay.codec.ArrowIpc.write(d.okay))))
    finally alloc.close()
  }

  test("ArrowIpc reads the stream Arrow Java writes, value for value") {
    val d = ArrowJava.data(10000)
    val alloc = RootAllocator()
    try assert(ArrowJava.same(d, ArrowJava.okayToArrays(ArrowJava.writeFromArrays(alloc, d))))
    finally alloc.close()
  }
