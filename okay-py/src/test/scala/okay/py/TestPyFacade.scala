package okay.py

import okay.given
import okay.py.golden.FacadeDemo

/** foreign-module-trait against a LIVE python3 (specs/foreign-highlevel.md stage 5) */
class TestPyFacade extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestPy.python.isEmpty

  /** where the golden module sits once test resources are on the classpath */
  private lazy val moduleDir =
    java.nio.file.Paths.get(getClass.getResource("/okay/py/golden/facadedemo.py").toURI).getParent
  private lazy val w = PySubprocess.start(TestPy.python.get, Map("PYTHONPATH" -> moduleDir.toString))
  private given okay.Handler[PyEval] = w.handler
  override def afterAll(): Unit = if TestPy.python.nonEmpty then w.close()

  private def golden: String =
    val rel = "src/test/scala/okay/py/golden/FacadeDemo.scala"
    val here = java.nio.file.Paths.get(rel)
    val file = if java.nio.file.Files.exists(here) then here else java.nio.file.Paths.get("okay-py", rel)
    java.nio.file.Files.readString(file)

  test("the checked-in facade is what the generator writes today") {
    val sigs = PyFacade.describe("facadedemo").runWith.toOption.get
    assertEquals(PyFacade.render("FacadeDemo", "okay.py.golden", "facadedemo", sigs), golden)
  }

  test("the generated facade calls Python, typed where Python said and open where it did not") {
    assertEquals(FacadeDemo.mean(Vector(1.0, 2.0, 6.0)).runWith, Right(3.0))
    assertEquals(FacadeDemo.label(7L).runWith, Right("item-7"))
    assertEquals(FacadeDemo.first(Vector.empty).runWith, Right(None))
    assertEquals(FacadeDemo.echo[String, String]("hi").runWith, Right("hi"))
  }
}

/** the annotation mapping (default gate) */
class TestPyFacadeTypes extends munit.FunSuite {
  test("type hints to Scala types; anything else stays open") {
    assertEquals(PyFacade.scalaType("list[Optional[float]]"), Some("Vector[Option[Double]]"))
    assertEquals(PyFacade.scalaType("int | None"), Some("Option[Long]"))
    assertEquals(PyFacade.scalaType("dict[str, int]"), None)
    assertEquals(PyFacade.scalaType(""), None)
  }

  test("a fully typed module imports neither Schema nor ToPy (an unused import would warn)") {
    val src = PyFacade.render("M", "p", "m", Vector(PySig("f", Vector(PyParam("x", "int", false)), "str", "")))
    assert(!src.contains("Schema") && !src.contains("ToPy"), src)
    assert(src.contains("import okay.py.{Condition, Py, PyEval}"), src)
  }
}
