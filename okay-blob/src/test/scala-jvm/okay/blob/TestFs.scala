package okay.blob

/** the filesystem engine against the contract — plus what only a
 * filesystem can get wrong: escape attempts and crash leftovers */
class TestFs extends BlobContract("fs") {

  def make(): Blob =
    Fs(java.nio.file.Files.createTempDirectory("okay-blob"))

  test("fs: a key cannot escape the root, and .tmp is reserved") {
    val root = java.nio.file.Files.createTempDirectory("okay-blob-esc")
    val b = Fs(root)
    val (_, out1, _) = drainGet(b.get("../../etc/passwd"))
    assert(out1.left.exists(_.contains("escapes")), out1.toString)
    assertEquals(run(b.head("../x")), None)
    val (_, out2, _) = drainGet(b.get("a.tmp"))
    assert(out2.left.exists(_.contains("reserved")))
    intercept[IllegalArgumentException](run(b.put("../evil", bytes(4))))
  }

  test("fs: a crash leftover (.tmp) is invisible to list and head") {
    val root = java.nio.file.Files.createTempDirectory("okay-blob-tmp")
    val b = Fs(root)
    val _ = run(b.put("real", bytes(4)))
    java.nio.file.Files.write(root.resolve("half.tmp"), Array[Byte](1, 2))
    assertEquals(drainList(b.list("")).map(_.key), Vector("real"))
  }
}
