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

  test("fs: a file goes in through putFile a chunk at a time, and comes back whole") {
    import okay.{Async, Chunk, Producer, Writer}
    val b = Fs(java.nio.file.Files.createTempDirectory("okay-blob-file"))
    val f = java.nio.file.Files.createTempFile("okay-bytes", ".bin")
    val data = Array.tabulate[Byte](200_001)(i => (i % 251).toByte)   // not a multiple of any chunk
    java.nio.file.Files.write(f, data)

    val _ = run(b.putFile("f/one", f, chunk = 8 * 1024))
    assertEquals(run(b.getBytes("f/one")).map(_.toVector), Right(data.toVector))

    // the producer never holds more than one chunk, and loses nothing
    var biggest = 0
    var total = 0L
    val _ = run(Producer.each[Chunk[Byte], Chunk[Byte], Async](Bytes.file(f, 8 * 1024)) { c =>
      biggest = math.max(biggest, c.length); total += c.length })
    assert(biggest <= 8 * 1024, biggest.toString)
    assertEquals(total, 200_001L)

    // and the same file on the Writer road
    val (told, _) = run(Writer.collect(Bytes.fileSource(f, 8 * 1024)))
    assertEquals(told.map(_.length).sum, 200_001)
    assertEquals(told.flatMap(_.toVector).toVector, data.toVector)
  }
}
