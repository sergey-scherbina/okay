# Load-owned thread lifecycle checks

## Overview

The publication gate reproduced TestLoadStress's JVM-wide thread-count assertion, not its burner-survival check. Threads started by the harness are outside Load's ownership. Load already clears its flag and joins the threads it creates in finally.

## Interface

No production API or behavior changes. TestLoadStress adopts Munit.Diagnosed.

## Behavior

- [ ] Capture the identities of the four newly created burners inside the body and assert that each is dead after the body returns.
- [ ] Check the same lifecycle when the body throws, retaining the original exception.
- [ ] Unrelated threads started during the body can remain alive without failing the owned-thread check; the fixture releases and joins them in finally.

## Decisions

Replace the JVM-wide count with checks on captured burner identities, rather than widening the count tolerance or adding sleeps. A controlled fixture starts three unrelated threads during the body, reproducing the previous assertion deterministically. Do not change Load merely to satisfy an assertion about resources it does not own.

## Out of scope

Changing interruption behavior, nested burner naming, or the production Load implementation.

## Results

Pending scoped reproduction and verification.
