# okay-cdi

> A Module's values as CDI beans through a portable Extension —
> `@Named` by the plan, singleton, the closer run at the container's
> shutdown — and a container's instance as a module.

Depends on: `okay` (JVM), jakarta.enterprise.cdi-api 4. Weld SE is
the test container; any CDI 4 container will do at runtime.

## Guide

**Rendering, not emulation** (specs/di.md). The container stays the
container; the module stays a value. The seam is the one Spring and
Guice use: `m.exports` plus `Resource.open`.

**Module → CDI.** `SeContainerInitializer.newInstance().addExtensions(OkayCdi.extension((db and pool).exports))`
builds the module now and, at `AfterBeanDiscovery`, adds one synthetic
bean per installed capability — `@Named` by the plan, typed by the
erased class — plus a `ModuleScope` bean for an explicit close. The
container's `BeforeShutdown` runs the same closer, so shutdown
releases in reverse acquisition order; the closer is idempotent.

**Singleton, not application-scoped.** The values are built; a normal
scope would hand out client proxies, a `final` class is not proxyable,
and a proxy is not the instance the module wired. `@Singleton` is the
pseudo-scope without proxies.

**CDI → Module.** `OkayCdi.instance[Clock](container)` is a module
whose acquisition selects by type when the scope builds.

## Gotchas

- `extension` is EAGER: acquisitions run when it is called, before
  the container exists.
- Two opaque roles over one class are two `@Named` beans and an
  AMBIGUOUS type: `select(classOf[Db])` says so, select by name.
- Weld SE wants a bean archive even with discovery off: an empty
  `META-INF/beans.xml` with `bean-discovery-mode="none"` (the tests
  carry one).
