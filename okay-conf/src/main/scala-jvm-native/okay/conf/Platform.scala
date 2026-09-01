package okay.conf

/** the JVM/Native half: real environment, real files */
private[conf] object Platform {

  val env: Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("env", name) =>
        Option(System.getenv(name)).toRight(s"'env:$name' is not set")
      case _ => Secrets.unrecognized(s)

  val file: Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("file", path) => slurp(path).map(trim1)
      case _ => Secrets.unrecognized(s)

  /** exactly one trailing newline trimmed — content ending in two
   * keeps one */
  private def trim1(content: String): String =
    if content.endsWith("\r\n") then content.dropRight(2)
    else if content.endsWith("\n") then content.dropRight(1)
    else content

  private[conf] def slurp(path: String): Either[String, String] =
    val p = java.nio.file.Paths.get(path)
    if java.nio.file.Files.isDirectory(p) then Left(s"'$path' is a directory")
    else if !java.nio.file.Files.exists(p) then Left(s"'$path' does not exist")
    else
      try Right(String(java.nio.file.Files.readAllBytes(p), "UTF-8"))
      catch case e: Exception => Left(s"'$path' cannot be read: ${e.getMessage}")
}
