package okay2.codec

/** The documents every JSON road is checked against, in ONE place
 * (okay-codec's JsonCorpus): TestJsonValue compares the two VALUE
 * roads over these, TestJsonCst and TestJsonStrict read them too. */
object JsonCorpus {

  val wellFormed: Seq[String] = Seq(
    """{"id":42,"user":"ada","amount":12.5,"active":true,"tags":["new","vip"],"addr":{"city":"Kyiv","zip":"01001","line":null},"note":"leave at door"}""",
    "{}", "[]", "0", "-0", "1", "-1.5e3", "1E+2", "1e-2", "123456789012345678", "1e999", "-1e999",
    "\"\"", "\"plain\"", "\"q\\\"uo\\\\te\\n\\t\\r\"", "\"\\u0041\"", "\"\\b\\f\\/\"", "\"日本語 ünïcödé\"",
    "true", "false", "null",
    " \n\t [ 1 , 2 , 3 ] \r\n", "{ \"a\" : { \"b\" : [ { } , [ ] , null ] } }",
    """{"a":1,"a":2}""", """[[[[[[1]]]]]]""", """{"k":"v","n":-0.0,"e":1.0E10}""",
    """[1,"two",true,null,{"three":3},[4]]""")

  val damaged: Seq[String] = Seq(
    "", "   ", "-", "[1,2,-", "{\"a\":-}", "-e5", "1e", ".", "+", "01", "1.", ".5", "1.e5",
    "[1,2,]", "{\"a\":1,}", "{\"a\"}", "{a:1}", "{'a':1}", "[1 2]", "1 2", "[1]]", "{\"a\":1}}",
    "\"unterminated", "\"raw\ncontrol\"", "\"tab\there\"", "tru", "nul", "[tru]", "{\"a\":nulls}",
    "\"esc at end\\", "[\"a\",]", "{\"a\":[1,2}", "[{\"a\":1]", "\u0000", "[1,2,3", "{\"a\":\"b\"")
}
