// okay2 is the project identity, not a version component.
@SuppressWarnings("module")
module okay2.probe {
    requires okay2.core;
    requires okay2.data;
    requires okay2.optics;
    requires okay2.stm;
    requires okay2.workflow;
    requires okay2.platform;
    requires java.sql;
    uses okay2.async.BlockingDefaults;
}
