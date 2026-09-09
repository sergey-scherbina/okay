package okay.guice

import okay.*
import com.google.inject.{AbstractModule, Injector, Module as GModule}
import com.google.inject.name.Names
import scala.reflect.ClassTag

/**
 * A Module inside Guice, both ways (specs/di.md, stage 2). Rendering,
 * not emulation: Guice stays the injector, the module stays a value.
 *
 * Module → Guice: `bindings` builds the module NOW (`Resource.open`)
 * and returns a Guice module binding each installed value by NAME
 * (the plan's — `@Named("Db")`) and, where its erased class occurs
 * once among the exports, by type too; two opaque roles over one
 * class are two names and no type binding, rather than a duplicate
 * Guice would refuse. Guice has no lifecycle, so the scope's closer
 * is bound as a `ModuleScope` instance: `injector.getInstance(classOf[ModuleScope]).close()`
 * releases in reverse order, at the moment the application chooses.
 *
 * Guice → Module: `instance[A]` is a module whose acquisition asks the
 * injector by type when the scope builds.
 */
object OkayGuice:

  /** the scope's closer, bound as an instance; idempotent */
  final class ModuleScope(closer: () => Unit):
    def close(): Unit = closer()

  /** what the module built, as Guice bindings; the closer bound beside them */
  def bindings(installed: Vector[Installed] ! Resource): GModule =
    val (xs, close) = Resource.open(installed)
    val once = xs.groupBy(_.cls).collect { case (c, Seq(_)) => c }.toSet
    new AbstractModule:
      override def configure(): Unit =
        xs.foreach(x => bindOne(x.name, x.cls, x.value, once(x.cls)))
        bind(classOf[ModuleScope]).toInstance(ModuleScope(close))
      /** `Installed` carries the erased class beside the value, and `Class.cast`
       * is the JVM's checked conversion to it — the one place the value
       * meets Guice's `Class[T]`-typed API */
      private def bindOne[T](name: String, cls: Class[T], value: Any, byType: Boolean): Unit =
        val v = cls.cast(value)
        bind(cls).annotatedWith(Names.named(name)).toInstance(v)
        if byType then bind(cls).toInstance(v)

  /** an injector's instance as a module, asked by type when the scope builds */
  def instance[A](injector: Injector)(using ct: ClassTag[A]): Module[[X] =>> A ?=> X] =
    module[A](ct.unapply(injector.getInstance(ct.runtimeClass)).getOrElse(
      throw IllegalStateException(s"instance of ${ct.runtimeClass.getName} is not one")))(_ => ())
