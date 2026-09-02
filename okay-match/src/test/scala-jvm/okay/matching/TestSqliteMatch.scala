package okay.matching

import okay.given
import okay.jdbc.JdbcSql
import java.sql.DriverManager

/** sqlite: a temp file per store; reopening is a second connection
 * to the same file */
class TestSqliteMatch extends MatchEngineSuite {
  def engine = "sqlite"
  def fresh(): (SqlMatch, () => SqlMatch) =
    val f = java.nio.file.Files.createTempFile("okay-match", ".db")
    def open() = SqlMatch(JdbcSql(DriverManager.getConnection(s"jdbc:sqlite:$f")))
    (open(), () => open())
}
