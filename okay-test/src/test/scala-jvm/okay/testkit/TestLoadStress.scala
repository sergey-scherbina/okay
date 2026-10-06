package okay.testkit

import okay.diagnose.Threads
import java.util.concurrent.CountDownLatch
import scala.jdk.CollectionConverters.*

class TestLoadStress extends Munit.Diagnosed {
  test("Stress counts failed rounds, keeps the first diagnoses, and runs rounds in parallel") {
    val r = Stress.repeat(100, parallel = 4, keep = 2)(i => if i % 10 == 0 then Some(s"bad $i") else None)
    assertEquals(r.failed, 10)
    assertEquals(r.first.size, 2)
    assert(!r.held)
    assert(Stress.repeat(20)(_ => None).held)
  }

  test("Load owns its burners, not unrelated threads started during its body") {
    for throwsBody <- Vector(false, true) do
      val before = Thread.getAllStackTraces.keySet.asScala.toSet
      val release = CountDownLatch(1)
      val unrelated = Vector.tabulate(3)(i => new Thread(() => release.await(), s"load-unrelated-$i"))
      var owned = Vector.empty[Thread]
      def body(): Int =
        owned = Thread.getAllStackTraces.keySet.asScala.iterator
          .filter(t => !before.contains(t) && t.getName.startsWith("okay-testkit-burner-")).toVector
        unrelated.foreach(_.start())
        if throwsBody then throw IllegalStateException("controlled failure")
        42
      onFailure(Threads.dump(_ => true))
      try
        if throwsBody then
          val error = intercept[IllegalStateException](Load.burners(4)(body()))
          assertEquals(error.getMessage, "controlled failure")
        else assertEquals(Load.burners(4)(body()), 42)
        note(s"throwsBody=$throwsBody owned=${owned.map(t => s"${t.getName}:${t.isAlive}")} unrelated=${unrelated.map(_.isAlive)}")
        assertEquals(owned.size, 4)
        assert(owned.forall(t => !t.isAlive), "a captured burner outlived its body")
        assert(unrelated.forall(_.isAlive), "unrelated fixture threads must still be alive")
      finally
        release.countDown()
        unrelated.foreach(_.join())
  }
}
