package okay2.http

/** every suite here binds a real port: out of `test`, in `integrationTest` */
trait Live extends munit.FunSuite {
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
}
