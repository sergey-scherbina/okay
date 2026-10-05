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

- [ ] Fresh WithKey calls transport the journal key and original intent
  is appended before the external request is performed.
- [ ] Remote success followed by failed journal completion leaves an
  incomplete intent; recovery sends the identical key and the provider
  applies one business action across both requests.
- [ ] Completed recovery and offline replay make no provider call;
  changed input is rejected before key injection or external execution.
- [ ] Tool adapters replace pre-existing key fields on the first call,
  preserve other arguments, and journal the original fingerprint.
- [ ] Failure of intent append prevents external execution; other
  first-attempt policies do not inject an idempotency key.

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

Pending red reproduction and scoped gates.
