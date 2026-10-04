package okay.java

import okay.java.examples.JavaCapabilities
import okay.testkit.Munit.Diagnosed

/**
 * Capabilities as the Java facade's static row (specs/java-capabilities.md),
 * every program written in `JavaCapabilities.java`.
 */
class TestJavaCapabilities extends munit.FunSuite, Diagnosed:

  test("form 1 through a capability"):
    assertEquals(JavaCapabilities.answered(), 42)

  test("form 2 through a capability"):
    assertEquals(JavaCapabilities.counted(), Stated[Integer, Integer](12, 21))

  test("form 3 through a capability, into a Var"):
    assertEquals(JavaCapabilities.intoVar(), Stated[Integer, Integer](2, 3))

  test("form 4 through a capability, resumed twice"):
    assertEquals(JavaCapabilities.allFlips(), java.util.List.of[Integer](3, 1, 2, 0))

  test("two instances of one effect: each handler takes its own capability's operations"):
    assertEquals(JavaCapabilities.nested(), 12)

  test("two Vars of one type, and of two types"):
    assertEquals(JavaCapabilities.twoInts(), "11 22 11")
    assertEquals(JavaCapabilities.intAndString(), "2 12")

  test("Env + Var + Raise: recover keeps the state reached"):
    assertEquals(JavaCapabilities.builtIns(5), Stated[Integer, String](6, "s=6"))
    assertEquals(JavaCapabilities.builtIns(200), Stated[Integer, String](201, "recovered too big: 201"))

  test("Io.run"):
    assertEquals(JavaCapabilities.io(), 42)

  test("an escaped capability is refused by name"):
    val e = intercept[IllegalStateException](JavaCapabilities.escaped())
    note(e.getMessage)
    assert(e.getMessage.contains("escaped its scope"), e.getMessage)

  test("an escaped capability is not taken by a new handler of the same effect"):
    val e = intercept[IllegalStateException](JavaCapabilities.escapedIntoAnother())
    note(e.getMessage)
    assert(e.getMessage.contains("escaped its scope"), e.getMessage)

  test("1 000 000 Var steps by Java recursion"):
    assertEquals(JavaCapabilities.deep(1000000), 1000000)
