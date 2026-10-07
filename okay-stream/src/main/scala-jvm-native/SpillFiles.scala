package okay


import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, EOFException, File, FileInputStream, FileOutputStream}

/**
 * Runs as temporary FILES (chunks-external-sort): each run one file in
 * `dir`, its records length-prefixed, written once through a buffer and
 * read back once; a run is deleted when the sort's output is read to its
 * end, and marked `deleteOnExit` so an abandoned output leaves nothing
 * behind the process either.
 */
final class SpillFiles(dir: File) extends Spill:
  def open(): Spill.Run =
    val f = File.createTempFile("okay-run-", ".bin", dir)
    f.deleteOnExit()
    val out = DataOutputStream(BufferedOutputStream(FileOutputStream(f), 1 << 16))
    new Spill.Run:
      def append(bytes: Array[Byte]): Unit = { out.writeInt(bytes.length); out.write(bytes) }
      def seal(): Unit = out.close()
      def records(): Iterator[Array[Byte]] = new Iterator[Array[Byte]]:
        private val in = DataInputStream(BufferedInputStream(FileInputStream(f), 1 << 16))
        private var nextLen: Int = read()
        private def read(): Int =
          try in.readInt() catch case _: EOFException => { in.close(); -1 }
        def hasNext: Boolean = nextLen >= 0
        def next(): Array[Byte] =
          val b = new Array[Byte](nextLen)
          in.readFully(b)
          nextLen = read()
          b
      def delete(): Unit = { val _ = f.delete() }

object SpillFiles:
  def temp: SpillFiles = SpillFiles(File(System.getProperty("java.io.tmpdir")))

/** where a sort spills by default on the platforms with a disk */
given spillToTempFiles: Spill = SpillFiles.temp
