# WithKey from the first external attempt

## Overview

Durable.over previously injected the journal key only when retrying an
incomplete intent. A provider could execute the fresh request without that
key and then execute its keyed retry again after completion recording
failed. This corrects the transport contract in the neutral handler;
key namespaces remain the separate durable-run-scoped-keys task.

## Interface

No signature or journal format changes. OnRepeat.WithKey applies
Journalled.withKey(op, Entry.key) before every external attempt, including
the first. Other first-attempt policies retain their behavior. The journal
stores the original operation fingerprint; replay/decoding uses that
original operation. Tool.withKey already replaces all supplied key fields
with the journal key: the journal's key is authoritative for transport.

## Behavior

- [x] Fresh WithKey calls transport the journal key and original intent
  is appended before the external request is performed.
- [x] Remote success followed by failed journal completion leaves an
  incomplete intent; recovery sends the identical key and the provider
  applies one business action across both requests.
- [x] Completed recovery and offline replay make no provider call;
  changed input is rejected before key injection or external execution.
- [x] Tool adapters replace pre-existing key fields on the first call,
  preserve other arguments, and journal the original fingerprint.
- [x] Failure of intent append prevents external execution; other
  first-attempt policies do not inject an idempotency key.

## Compatibility prerequisite found by the affected gate

The prior module extraction left TestDurableForeign importing agent
through the now-neutral Python test graph, and Scala 2's TASTy reader
rejects the exported enum type OnRepeat with addChild inapplicable.
Before landing this fix, migrate that remaining generic consumer to the
neutral import and use an explicit type/value alias for the enum in the
agent facade. Verify the actual Scala 2 probe, not only its Scala 3 bridge.
No recovery policy or wire enum value changes are intended.

## Decisions

Use one per-operation policy selection for fresh calls and inject only
WithKey. Do not fingerprint the modified transport request: that would
change replay comparisons and invalidate existing histories. Keep
legacy stored Entry.key authoritative on retry. Provider idempotency is
an assumption tested with a deduplicating fake, not an exactly-once
network transport promise. These are exception/crash-window tests, not
process-kill or power-loss tests.

## Validation

Portable neutral WithKey tests on JVM/JS, a Tool-adapter compatibility
regression suite, then affected master staged. No real payment API.

## Results

Red reproduction: TestWithKey failed twice on the old handler. The crash
case recorded requests None then Some(journalKey), two provider actions,
and receipt-2 on retry. After the fix, the four neutral scenarios pass
on JVM and JS, and the Tool key-precedence regression passes (9 results).
Affected staged gate pending before landing.
