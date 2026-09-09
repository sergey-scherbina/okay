package okay.cdi

import okay.*
import jakarta.inject.Singleton
import jakarta.enterprise.event.Observes
import jakarta.enterprise.inject.Instance
import jakarta.enterprise.inject.literal.NamedLiteral
import jakarta.enterprise.inject.spi.{AfterBeanDiscovery, BeforeShutdown, Extension}
import scala.annotation.unused
import scala.reflect.ClassTag

/**
 * A Module inside a CDI container, both ways (specs/di.md, stage 2).
 * Rendering, not emulation: the container stays the container, the
 * module stays a value — the same seam Spring and Guice use,
 * `m.exports` plus `Resource.open`.
 *
 * Module → CDI: `extension(installed)` is a portable Extension that
 * builds the module NOW and, at `AfterBeanDiscovery`, adds one
 * synthetic `@Singleton` bean per installed capability —
 * `@Named` by the plan, typed by the erased class — plus a
 * `ModuleScope` bean for an explicit close; the container's
 * `BeforeShutdown` runs the same closer, so shutdown releases in
 * reverse order as the region would.
 * `@Singleton`, the pseudo-scope, because the values ARE built: a
 * normal scope would hand out client proxies — a `final` class is not
 * proxyable, and a proxy is not the instance the module wired.
 * Two opaque roles over one class are two names; by type they are
 * ambiguous, and CDI says so at resolution, as it should.
 *
 * CDI → Module: `instance[A]` is a module whose acquisition selects
 * by type when the scope builds.
 */
object OkayCdi:

  /** the scope's closer as a bean; destroyed with the application context */
  final class ModuleScope(closer: () => Unit):
    def close(): Unit = closer()

  /** the extension: pass it to `SeContainerInitializer.addExtensions` */
  def extension(installed: Vector[Installed] ! Resource): Extension =
    val (xs, close) = Resource.open(installed)
    Beans(xs, close)

  /** a public class with a public observer: the container finds
   * `addBeans` by reflection, so it cannot be an anonymous member */
  final class Beans(xs: Vector[Installed], close: () => Unit) extends Extension:
    def addBeans(@Observes abd: AfterBeanDiscovery): Unit =
      xs.foreach(x => one(abd, x.name, x.cls, x.value))
      abd.addBean[ModuleScope]()
        .types(classOf[ModuleScope])
        .scope(classOf[Singleton])
        .createWith(_ => ModuleScope(close))
      ()
    /** the container's shutdown IS the end of the scope — observed
     * directly, because a singleton bean nobody selected is never
     * instantiated and so never destroyed; the closer is idempotent,
     * so an explicit `ModuleScope.close()` before shutdown is fine */
    def stop(@Observes @unused bs: BeforeShutdown): Unit = close()

  /** `Installed` carries the erased class beside the value, and `Class.cast`
   * is the JVM's checked conversion to it — the one place the value
   * meets CDI's `Class[T]`-typed API */
  private def one[T](abd: AfterBeanDiscovery, name: String, cls: Class[T], value: Any): Unit =
    val v = cls.cast(value)
    abd.addBean[T]()
      .types(cls)
      .qualifiers(NamedLiteral.of(name))
      .scope(classOf[Singleton])
      .createWith(_ => v)
    ()

  /** a container's instance as a module, selected by type when the scope builds */
  def instance[A](container: Instance[AnyRef])(using ct: ClassTag[A]): Module[[X] =>> A ?=> X] =
    module[A](ct.unapply(container.select(ct.runtimeClass).get()).getOrElse(
      throw IllegalStateException(s"instance of ${ct.runtimeClass.getName} is not one")))(_ => ())
