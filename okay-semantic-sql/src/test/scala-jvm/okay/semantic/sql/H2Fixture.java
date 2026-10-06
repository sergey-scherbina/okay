package okay.semantic.sql;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

/** Test-only optional adapter; independent of service discovery and driver registration. */
public final class H2Fixture {
    private H2Fixture() {}

    public static Connection open() throws SQLException {
        return new org.h2.Driver().connect("jdbc:h2:mem:", new Properties());
    }

    /** Only a child JVM runs this: no shared suite's JDBC registry is changed. */
    public static void main(String[] args) throws Exception {
        try (Connection ignored = open()) {
            // Load H2, then remove service registrations in this isolated process.
        }
        var registered = DriverManager.getDrivers();
        while (registered.hasMoreElements()) {
            DriverManager.deregisterDriver(registered.nextElement());
        }
        try (Connection ignored = DriverManager.getConnection("jdbc:h2:mem:")) {
            throw new AssertionError("registry unexpectedly discovered H2");
        } catch (SQLException expected) {
            if (!expected.getMessage().contains("No suitable driver")) throw expected;
        }
        try (Connection connection = open();
             var statement = connection.createStatement();
             var result = statement.executeQuery("SELECT 42")) {
            if (!result.next() || result.getInt(1) != 42) {
                throw new AssertionError("direct fixture failed to execute query");
            }
        }
        System.out.println("registry absent; direct query returned 42");
    }
}
