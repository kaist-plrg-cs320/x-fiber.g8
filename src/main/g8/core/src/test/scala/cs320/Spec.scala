package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  test(run("42"), "42")
  test(run("1 + 2"), "3")
  test(run("7 - 2"), "5")
  test(run("2 * 4"), "8")
  test(run("5 / 2"), "2")
  test(run("13 % 5"), "3")
  test(run("1 - -1"), "2")
  test(run("true"), "true")
  test(run("1 == 3 - 2"), "true")
  test(run("1 < 3 - 2"), "false")
  test(run("(1, 2 + 3, true)"), "(1, 5, true)")
  test(run("((42, 3 * 2), false)"), "((42, 6), false)")
  test(run("(1, 2 + 3, true)._1"), "1")
  test(run("((42, 3 * 2), false)._1._2"), "6")
  test(run("Nil"), "Nil")
  test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
  test(run("Nil.isEmpty"), "true")
  test(run("(1 :: Nil).isEmpty"), "false")
  test(run("(1 :: Nil).head"), "1")
  test(run("(1 :: Nil).tail"), "Nil")
  test(run("(1 :: 2 :: Nil).tail.head"), "2")
  test(run("""
    val x = 1 + 2;
    val y = x * 4 + 1;
    y / (x - 1)
  """), "6")
  test(run("""
    val (x, y) = (1 + 2, 3 + 4);
    val z = x * y;
    val (a, b, c) = (z, z + z, z + z + z);
    c - b
  """), "21")
  test(run("x => x + x"), "<function>")
  test(run("(x => x + x)(1)"), "2")
  test(run("(x => y => x + y)(1)(2)"), "3")
  test(run("((x, y) => x + y)(1, 2)"), "3")
  test(run("1.isInstanceOf[Int]"), "true")
  test(run("1.isInstanceOf[Boolean]"), "false")
  test(run("(1 :: Nil).isInstanceOf[List]"), "true")
  test(run("(x => x + x).isInstanceOf[Function]"), "true")
  test(run("if (true) 1 else 2"), "1")
  test(run("!true"), "false")
  test(run("true && false"), "false")
  test(run("true || false"), "true")
  test(run("1 != 2"), "true")
  test(run("1 <= 1"), "true")
  test(run("1 > 1"), "false")
  test(run("1 >= 1"), "true")
  test(run("Nil.nonEmpty"), "false")
  test(run("""
    def f(x) = x - 1;
    f(2)
  """), "1")
  test(run("""
    def f(x) = if (x < 1) 0 else x + f(x - 1);
    f(10)
  """), "55")
  test(run("""
    vcc x;
    1 + x(1) + 1
  """), "1")
  test(run("""
    (x => x * x)(
      1 + vcc x; 1 + x(2) + 3
    )
  """), "9")
  test(run("(x => (return 1) + x)(2)"), "1")
  test(run("""
    def div(x) = (x => 10 / x)(
      if (x == 0) return 0 else x
    );
    div(0) + div(10)
  """), "1")
  testExc(run("throw 1"), "")
  testExc(run("throw throw 1"), "")
  test(run("""
    try {
      throw 1
    } catch (
      x => x + x
    )
  """), "2")
  test(run("""
    1 + vcc x;
      try {
        throw 1
      } catch x
  """), "2")

  /* Write your own tests */
}
