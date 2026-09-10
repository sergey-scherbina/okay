package okay.flink.wroclaw;

import org.apache.flink.api.common.accumulators.Accumulator;
import org.apache.flink.api.common.accumulators.SimpleAccumulator;

/**
 * A Flink accumulator that XORs — how the lanes' ORDER-INDEPENDENT
 * checksums come back from the cluster. Flink ships a LongCounter for
 * sums and nothing for this, and a sum would not do: two lanes must
 * agree on a hash of every emitted record regardless of the order the
 * subtasks emitted them in, which is exactly what XOR gives.
 */
public class XorLong implements SimpleAccumulator<Long> {
    private static final long serialVersionUID = 1L;

    private long value = 0L;

    @Override
    public void add(Long v) {
        value ^= v;
    }

    @Override
    public Long getLocalValue() {
        return value;
    }

    @Override
    public void resetLocal() {
        value = 0L;
    }

    @Override
    public void merge(Accumulator<Long, Long> other) {
        value ^= other.getLocalValue();
    }

    @Override
    public XorLong clone() {
        XorLong c = new XorLong();
        c.value = value;
        return c;
    }
}
