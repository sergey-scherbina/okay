package okay.bayes

import okay.testkit.Munit.Diagnosed

/** specs/okay-bayes.md stage 4: samplers by name */
class TestSamplers extends Diagnosed:
  test("byName: ours, PyMC when okay-py is on the classpath, anything else refused naming the choices") {
    assertEquals(Samplers.byName("okay"), Right(Sampler.Okay))
    assertEquals(PyMC.missing, None)
    assertEquals(Samplers.byName("pymc"), Right(PyMC))
    assertEquals(Samplers.byName("stan"), Left("no sampler 'stan': the choices are okay, pymc"))
  }
