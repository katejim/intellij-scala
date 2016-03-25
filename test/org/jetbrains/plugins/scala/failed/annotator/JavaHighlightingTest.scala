package org.jetbrains.plugins.scala.failed.annotator

import org.jetbrains.plugins.scala.PerfCycleTests
import org.jetbrains.plugins.scala.javaHighlighting.JavaHighltightingTestBase
import org.junit.experimental.categories.Category

/**
  * @author Alefas
  * @since 23/03/16
  */
@Category(Array(classOf[PerfCycleTests]))
class JavaHighlightingTest extends JavaHighltightingTestBase {
  def testSCL9663B() = {
    val scala =
      """
        |class Foo(val cell: String) extends AnyVal {
        |  def foo(x: Int) = 123
        |}
      """.stripMargin
    val java =
      """
        |public class Test {
        |    public static void main(String[] args) {
        |        Foo$ foo = Foo$.MODULE$;
        |
        |        foo.foo$extension("text", 1);
        |    }
        |}
      """.stripMargin
    assertNoErrors(messagesFromJavaCode(scala, java, "Test"))
  }

  def testSCL7525() = {
    val scala =
      """
        |package SCL7525
        |object Test {
        |  new Foo(new Foo.ArgsBar)
        |}
      """.stripMargin

    val java =
      """
        |package SCL7525;
        |public class Foo {
        |    public Foo(Args a) { }
        |    public static class Args<T extends Args<T>> { }
        |    public static class ArgsBar extends Args<ArgsBar> { }
        |}
      """.stripMargin

    assertNoErrors(messagesFromScalaCode(scala, java))
  }

  def testSCL9029() = {
    val scala =
      """
        |package scl9029
        |import java.lang.invoke.{MethodHandles, MethodType}
        |
        |class SCL9029 {
        |  def a: Int = 5
        |
        |  def b = {
        |    val mh = MethodHandles.publicLookup().findVirtual(
        |      classOf[A], "a", MethodType.methodType(classOf[Int])
        |    )
        |    val z: Int = mh.invokeExact(this)
        |  }
        |}
      """.stripMargin

    val java =
      """
        |package scl9029;
        |public class Foo {
        |}
      """.stripMargin

    assertNoErrors(messagesFromScalaCode(scala, java))
  }
}