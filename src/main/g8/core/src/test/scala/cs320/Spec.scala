package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  test(run("({ vcc q; (q(26) + 26) } + 6)"), "32")
  test(run("({ vcc k; (k(26) * 26) } * 6)"), "156")
  test(run("({ vcc t; (t(26) / 26) } / 6)"), "4")
  test(run("(26 / (-6))"), "-4")
  test(run("((-26) / 6)"), "-4")
  test(run("((-26) / (-6))"), "4")
  test(run("({ vcc l; (l(26) % 26) } % 6)"), "2")
  test(run("((-26) % 6)"), "-2")
  test(run("(6 == 6)"), "true")
  test(run("({ vcc v; (v(26) == 26) } == 6)"), "false")
  test(run("(6 < 6)"), "false")
  test(run("({ vcc m; (m(26) < 26) } < 6)"), "false")
  test(run("{if ({ vcc q; {if (q(true)) 26 else 6 } }) 6 else 26 }"), "6")
  test(run("{if (true) 6 else x }"), "6")
  test(run("{if (false) 6 else 26 }"), "26")
  test(run("({ vcc l; (l(6), 6) }, 26)"), "(6, 26)")
  test(run("(6, 26)._1"), "6")
  test(run("{ vcc n; n((6, 26))._1 }._2"), "26")
  test(run("(26 :: { vcc p; (6 :: p((6 :: Nil))) })"), "(26 :: (6 :: Nil))")
  test(run("(Nil :: Nil)"), "(Nil :: Nil)")
  test(run("Nil.isEmpty"), "true")
  test(run("{ vcc a; (6 :: a(Nil)) }.isEmpty"), "true")
  test(run("(6 :: Nil).isEmpty"), "false")
  test(run("{ vcc o; o((6 :: Nil)).head }.isEmpty"), "false")
  test(run("(6 :: Nil).head"), "6")
  test(run("{ vcc m; m((6 :: Nil)).head }.head"), "6")
  test(run("(26 :: (6 :: Nil)).tail"), "(6 :: Nil)")
  test(run("{ vcc x; x((26 :: (6 :: Nil))).tail }.tail"), "(6 :: Nil)")
  test(run("{ val x = true; { val x = 26; x } }"), "26")
  test(run("{ val x = 6; { val f = x; f } }"), "6")
  test(run("(() => 6)()"), "6")
  test(run("((x, f) => (x + f))(6, 26)"), "32")
  test(run("((x) => ((f) => (x + f)))(6)(26)"), "32")
  test(run("{ val x = 6; { def x() = x; x } }"), "<function>")
  test(run("{ val x = 6; { def x() = x; x() } }"), "<function>")
  test(run("{ val f = 6; { def x() = f; x() } }"), "6")
  test(run("{ def u(z) = {if ((z < 1)) (z + 1) else (u((z + (-1))) + (z + 4)) }; u(12) }"), "127")
  test(run("{ def z(u, a) = {if ((u < 2)) (a + 4) else (f((u + 5), (u + (-1)), (a + 3)) + (u + 2)) }; def t(g, l) = {if ((l < 3)) (g + 1) else (z((l + (-1)), (l + 2)) + (g + 2)) }; def f(d, e, i) = {if ((e < 2)) (d + 2) else (t((d + 1), (e + (-1))) + (d + 5)) }; t(14, 14) }"), "197")
  test(run("(6.isInstanceOf[Int], false.isInstanceOf[Int], (6, 26).isInstanceOf[Int], Nil.isInstanceOf[Int], (6 :: Nil).isInstanceOf[Int], ((b) => b).isInstanceOf[Int], { vcc x; x.isInstanceOf[Int] }, 6.isInstanceOf[Boolean], false.isInstanceOf[Boolean], (6, 26).isInstanceOf[Boolean], Nil.isInstanceOf[Boolean], (6 :: Nil).isInstanceOf[Boolean], ((b) => b).isInstanceOf[Boolean], { vcc x; x.isInstanceOf[Boolean] }, 6.isInstanceOf[Tuple], true.isInstanceOf[Tuple], (6, 26).isInstanceOf[Tuple], Nil.isInstanceOf[Tuple], (6 :: Nil).isInstanceOf[Tuple], ((t) => t).isInstanceOf[Tuple], { vcc x; x.isInstanceOf[Tuple] }, 6.isInstanceOf[List], true.isInstanceOf[List], (6, 26).isInstanceOf[List], Nil.isInstanceOf[List], (6 :: Nil).isInstanceOf[List], ((e) => e).isInstanceOf[List], { vcc x; x.isInstanceOf[List] }, 6.isInstanceOf[Function], true.isInstanceOf[Function], (6, 26).isInstanceOf[Function], Nil.isInstanceOf[Function], (6 :: Nil).isInstanceOf[Function], ((a) => a).isInstanceOf[Function], { vcc x; x.isInstanceOf[Function] })"), "(true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, true, true)")
  test(run("({ vcc v; v(6) }.isInstanceOf[Int], { vcc r; r(true) }.isInstanceOf[Int])"), "(true, false)")
  test(run("{ vcc f; (true + f(6)) }"), "6")
  test(run("{ vcc q; (q(6) + q(26)) }"), "6")
  test(run("{ vcc n; 6(n(26)) }"), "26")
  test(run("{ vcc k; ((x, f) => (x + f))(k(6), k(26)) }"), "6")
  test(run("{ try 6 catch 26 }"), "6")
  test(run("{ try { throw 6 } catch ((u) => u) }"), "6")
  test(run("{ try { throw { throw 6 } } catch ((k) => k) }"), "6")
  test(run("{ try { throw 6 } catch { vcc h; h(((o) => o))(26) } }"), "6")
  test(run("{ try { try { throw 6 } catch ((c) => c) } catch ((x) => (x + x)) }"), "6")
  test(run("{ try ({ throw 6 } + { throw 26 }) catch ((q) => q) }"), "6")
  test(run("{ try { try { throw 26 } catch { throw 6 } } catch ((m) => m) }"), "6")
  test(run("{ val x = (() => { throw 6 }); { try x() catch ((m) => m) } }"), "6")
  testExc(run("{ val x = 6; f }"), "")
  testExc(run("(6 + true)"), "")
  testExc(run("(6 * true)"), "")
  testExc(run("(6 / true)"), "")
  testExc(run("(6 / ((t) => t)(0))"), "")
  testExc(run("(6 % false)"), "")
  testExc(run("(6 % ((m) => m)(0))"), "")
  testExc(run("(6 == false)"), "")
  testExc(run("(6 < true)"), "")
  testExc(run("{if (6) 6 else 26 }"), "")
  testExc(run("(26 :: (6 :: Nil))._1"), "")
  testExc(run("(6, 26)._3"), "")
  testExc(run("(26 :: 6)"), "")
  testExc(run("(6, 26).isEmpty"), "")
  testExc(run("(6, 26).head"), "")
  testExc(run("(6, 26).tail"), "")
  testExc(run("((q) => q)(Nil).head"), "")
  testExc(run("((p) => p)(Nil).tail"), "")
  testExc(run("{ val f = (() => x); { val x = 6; f() } }"), "")
  testExc(run("6()"), "")
  testExc(run("((k) => k)()"), "")
  testExc(run("((o) => o)(6, 26)"), "")
  testExc(run("({ try 6 catch ((h) => h) } + { throw 26 })"), "")
  test(run("""
  def isPrime(n) =
    if (!n.isInstanceOf[Int] || n <= 1) throw n
    else
      def aux(m) =
        if (n == m) return true
        else if (n % m == 0) return false
        else return aux(m + 1);
      return aux(2);
  def factorize(n) =
    if (!n.isInstanceOf[Int] || n <= 0) throw n
    else if (isPrime(n)) return n :: Nil
    else
      def aux(m) =
        if (n % m == 0) return m :: factorize(n / m)
        else return aux(m + 1);
      return aux(2);
  factorize(936)
  """), "(2 :: (2 :: (2 :: (3 :: (3 :: (13 :: Nil))))))")
  test(run("""
  def merge(l, r) =
    (if (l.isEmpty) return r else 0) ::
    (if (r.isEmpty) return l else 0) ::
    (
      val x = l.head;
      val y = r.head;
      (if (x <= y) return x :: merge(l.tail, r) else 0) ::
      (return y :: merge(l, r.tail))
    );
  def split(o) =
    (if (o.isEmpty) return (Nil, Nil) else 0) ::
    (if (o.tail.isEmpty) return (o, Nil) else 0) ::
    (
      val x = o.head;
      val y = o.tail.head;
      val zs = o.tail.tail;
      val (xs, ys) = split(zs);
      return (x :: xs, y :: ys)
    );
  def mergeSort(o) =
    (if (o.isEmpty) return Nil else 0) ::
    (if (o.tail.isEmpty) return o else 0) ::
    (
      val (as, bs) = split(o);
      return merge(mergeSort(as), mergeSort(bs))
    );
  mergeSort(7 :: 10 :: 9 :: 6 :: 8 :: Nil)
  """), "(6 :: (7 :: (8 :: (9 :: (10 :: Nil)))))")
  test(run("""
  def break(x) = throw (Nil, x);
  def loop(f, init) =
    def aux(n) = aux(f(n));
    try
      aux(init)
    catch
      x =>
        if (x.isInstanceOf[Tuple] && x._1.isInstanceOf[List]) x._2
        else throw x;
  def f(p) =
    if (p._1 <= 0) break(p)
    else (p._1 - 1, p._1 + p._2);
  val sum = n => loop(f, (n, 0))._2;
  sum(12)
  """), "78")
  test(run("""
  def Some(x) = (true, x);
  val None = (false, 0);
  def get(opt) =
    if (opt.isInstanceOf[Tuple] && opt._1.isInstanceOf[Boolean] && opt._1) opt._2
    else throw opt;
  def flatMap(opt, f) =
    if (opt.isInstanceOf[Tuple] && opt._1.isInstanceOf[Boolean])
      if (opt._1) f(opt._2)
      else opt
    else throw opt;
  def lift(f) = x =>
    try
      Some(f(x))
    catch
      x => None;
  def div100(x) =
    if (x.isInstanceOf[Int] && x != 0) 100 / x
    else throw x;
  val safeDiv = lift(div100);
  get(flatMap(safeDiv(6), safeDiv)) +
    try
      get(flatMap(safeDiv(0), safeDiv))
    catch
      x => 0
  """), "6")
  test(run("""
  def len(l) = if (l.isEmpty) 0 else 1 + len(l.tail);
  val yin =
    val k = (vcc x; x);
    if (k.isInstanceOf[Function]) (k, Nil)
    else if (len(k._2) < 10) (k._1, 6 :: k._2)
    else ((x) => x, k._2);
  val yang =
    val k = (vcc x; x);
    if (k.isInstanceOf[Function]) (k, yin._2)
    else (k._1, 2 :: k._2);
  val r = yin._1(yang);
  yin._2
  """), "(2 :: (2 :: (2 :: (2 :: (6 :: (2 :: (2 :: (2 :: (6 :: (2 :: (2 :: (6 :: (2 :: (6 :: Nil))))))))))))))")

  /* Write your own tests */
}
