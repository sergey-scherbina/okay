package okay.conf

/**
 * specs/conf.md, "The four that shell out": what a reference must
 * look like, decided before any process starts.
 *
 * Everything that actually RUNS something is `TestSchemesLive` —
 * including, deliberately, the assertions about which command each
 * builds, since seeing the command means running one.
 */
class TestSchemes extends munit.FunSuite:

  test("each resolver answers only its own scheme, and shrugs at the others") {
    val mine = Vector("sops:a.yaml#k", "aws-sm:id", "gcp-sm:name", "azure-kv:v/n")
    val four = Vector(Schemes.sops(), Schemes.awsSecrets(), Schemes.gcpSecrets(), Schemes.azureVault())
    for r <- mine do
      // exactly one of the four owns it; the other three say so and
      // never guess
      val shrugs = four.map(_.get(Secret(r)).left.getOrElse("")).count(_.startsWith("unrecognized scheme"))
      assertEquals(shrugs, 3, s"$r")
    // a scheme none of them serves is four shrugs, not a silence
    for other <- Vector("env:X", "file:/x", "nonsense:x") do
      val shrugs = four.map(_.get(Secret(other)).left.getOrElse("")).count(_.startsWith("unrecognized scheme"))
      assertEquals(shrugs, 4, other)
  }

  test("a reference that names nothing is refused with the shape it should have had") {
    assert(Schemes.sops().get(Secret("sops:#key")).left.exists(_.contains("sops:<file>#<key>")))
    assert(Schemes.awsSecrets().get(Secret("aws-sm:")).left.exists(_.contains("aws-sm:<id or arn>")))
    assert(Schemes.gcpSecrets().get(Secret("gcp-sm:")).left.exists(_.contains("gcp-sm:<name>[#<version>]")))
  }

  test("azure-kv needs a vault AND a name, because a deployment reading two vaults is normal") {
    for bad <- Vector("azure-kv:novaultname", "azure-kv:/name", "azure-kv:vault/", "azure-kv:") do
      assert(Schemes.azureVault().get(Secret(bad)).left.exists(_.contains("<vault>/<name>")), bad)
  }

  test("a refusal names the reference and never a value — there is no value to name yet") {
    val m = Schemes.azureVault().get(Secret("azure-kv:oops")).left.getOrElse("")
    assert(m.contains("azure-kv:oops"), m)
  }

  test("`all` is the chain okay-deploy's doctor describes, and env still wins for env:") {
    val ref = "env:PATH"
    assert(Schemes.all().get(Secret(ref)).isRight, "env: stopped resolving once the chain grew")
  }
