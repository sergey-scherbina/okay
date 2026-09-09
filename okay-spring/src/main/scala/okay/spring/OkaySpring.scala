package okay.spring

import okay.*
import org.springframework.beans.factory.{BeanFactory, DisposableBean}
import org.springframework.context.support.GenericApplicationContext
import java.util.function.Supplier
import scala.reflect.ClassTag

/**
 * A Module inside a Spring context, both ways (specs/di.md, stage 2).
 * Rendering, not emulation: Spring stays the container, the module
 * stays a value.
 *
 * Module → Spring: `register` builds the module NOW (`Resource.open`)
 * and registers one singleton per installed capability, named by the
 * plan and typed by the erased class, plus one `DisposableBean` whose
 * destroy is the scope's closer — so the context's close releases in
 * reverse order, as the region would. Call it before `refresh`, from
 * an `ApplicationContextInitializer` or `SpringApplication.addInitializers`.
 *
 * Spring → Module: `bean[A]` is a module whose acquisition looks the
 * bean up by type when the scope builds — a container is a source of
 * values for a `Providing`, nothing more.
 */
object OkaySpring:

  /** the scope's closer as a bean: Spring destroys it when the context closes */
  final class ModuleScope(close: () => Unit) extends DisposableBean:
    def destroy(): Unit = close()

  /** register what the module builds as singletons; the scope closes with the context */
  def register(ctx: GenericApplicationContext, installed: Vector[Installed] ! Resource): Unit =
    val (xs, close) = Resource.open(installed)
    xs.foreach(x => one(ctx, x.name, x.cls, x.value))
    ctx.registerBean("okayModuleScope", classOf[ModuleScope], (() => ModuleScope(close)): Supplier[ModuleScope])

  /** `Installed` carries the erased class BESIDE the value, and `Class.cast`
   * is the JVM's checked conversion to it — the one place the value
   * meets Spring's `Class[T]`-typed API */
  private def one[T](ctx: GenericApplicationContext, name: String, cls: Class[T], value: Any): Unit =
    ctx.registerBean(name, cls, (() => cls.cast(value)): Supplier[T])

  /** a Spring bean as a module: looked up by type when the scope builds */
  def bean[A](factory: BeanFactory)(using ct: ClassTag[A]): Module[[X] =>> A ?=> X] =
    module[A](ct.unapply(factory.getBean(ct.runtimeClass)).getOrElse(
      throw IllegalStateException(s"bean of ${ct.runtimeClass.getName} is not a ${ct.runtimeClass.getName}")))(_ => ())
