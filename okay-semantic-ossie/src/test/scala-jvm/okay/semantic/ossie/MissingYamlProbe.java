package okay.semantic.ossie;

/** This child JVM has our classes and Scala/codec, but deliberately no YAML jar. */
public final class MissingYamlProbe {
    private MissingYamlProbe() {}
    public static void main(String[] args) throws Exception {
        try {
            Class.forName("org.snakeyaml.engine.v2.api.LoadSettings");
            throw new AssertionError("probe unexpectedly has the optional dependency");
        } catch (ClassNotFoundException expected) {
            // The adapter must still load and refuse by name.
        }
        Class<?> adapter = Class.forName("okay.semantic.ossie.SnakeYaml$");
        Object module = adapter.getField("MODULE$").get(null);
        Object result = adapter.getMethod("byName", String.class).invoke(module, "snakeyaml");
        String verdict = result.toString();
        if (!verdict.contains("Left") || !verdict.contains("requires optional org.snakeyaml")) {
            throw new AssertionError(verdict);
        }
        System.out.println(verdict);
    }
}
