package okay.testkit

import okay.diagnose.Threads

class TestLoadStress extends munit.FunSuite {
  test("Stress counts failed rounds, keeps the first diagnoses, and runs rounds in parallel") {
    val r = Stress.repeat(100, parallel = 4, keep = 2)(i => if i % 10 == 0 then Some(s"bad $i") else None)
    assertEquals(r.failed, 10)
    assertEquals(r.first.size, 2)
    assert(!r.held)
    assert(Stress.repeat(20)(_ => None).held)
  }

  test("Load's burners stop when the body throws") {
    val before = Thread.getAllStackTraces.keySet.size
    intercept[IllegalStateException](Load.burners(4)(throw IllegalStateException("x"))): Unit
    val burners = Threads.dump(_.getName.startsWith("okay-testkit-burner"))
    assertEquals(burners, "", "a burner outlived its body")
    assert(Thread.getAllStackTraces.keySet.size <= before + 1)
  }
}
