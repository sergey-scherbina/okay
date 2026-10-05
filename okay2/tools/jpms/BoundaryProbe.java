package okay2.probe;

import java.util.ServiceLoader;
import okay2.async.BlockingDefaults;
import okay2.async.BlockingProviders;
import okay2.async.Handoff;

public final class BoundaryProbe {
    private BoundaryProbe() {}

    private static void require(boolean condition, String diagnosis) {
        if (!condition) throw new AssertionError(diagnosis);
    }

    public static void main(String[] args) throws ClassNotFoundException {
        boolean classpath = args.length == 1 && args[0].equals("classpath");
        Module core = Class.forName("okay2.Free").getModule();
        Module sql = ModuleLayer.boot().findModule("java.sql").orElseThrow();
        require(core.isNamed() != classpath, "unexpected core loading mode: " + core);
        if (!classpath) require(!core.canRead(sql), "core unexpectedly reads SQL");
        String[][] ownership = {
            {"okay2.data.Hlc", "okay2.data"},
            {"okay2.optics.Optic", "okay2.optics"},
            {"okay2.stm.Stm", "okay2.stm"},
            {"okay2.workflow.Wf", "okay2.workflow"}
        };
        for (String[] entry : ownership) {
            Module owner = Class.forName(entry[0]).getModule();
            require(classpath ? !owner.isNamed() : entry[1].equals(owner.getName()), entry[0] + " belongs to " + owner);
        }
        var providers = ServiceLoader.load(BlockingDefaults.class).stream().toList();
        require(providers.size() == 1, "expected one platform provider: " + providers);
        BlockingDefaults platform = providers.get(0).get();
        Module provider = platform.getClass().getModule();
        require(classpath ? !provider.isNamed() : "okay2.platform".equals(provider.getName()), "unexpected provider owner: " + provider);
        require(BlockingProviders.discover().size() == 1, "async uses declaration does not discover provider");
        require(platform.timer() != null && platform.scheduler() != null, "incomplete platform capabilities");
        Handoff<String> handoff = platform.canBlock().handoff();
        handoff.got("delivered");
        platform.canBlock().await(handoff);
        require(handoff.answer().get().equals("delivered"), "provider handoff did not deliver its answer");
        if (args.length == 1 && args[0].equals("interop")) {
            for (String adapter : new String[]{"okay2.cats.Io$", "okay2.fs2.Fs2Interop$", "okay2.zio.Zio$",
                    "cats.effect.IO", "fs2.Stream", "zio.ZIO", "zio.stream.ZStream"}) {
                require(Class.forName(adapter).getDeclaredMethods().length > 0, "empty interop API: " + adapter);
            }
        }
        System.out.println(classpath ? "JPMS: classpath service compatibility PASS" :
                "JPMS: named ownership, SQL isolation and platform service PASS");
    }
}
