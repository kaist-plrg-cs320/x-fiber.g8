package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  test(run("(31 + 7)"), "38")
  test(run("({ vcc f; (f(31) + 31) } + 7)"), "38")
  test(run("(31 * 7)"), "217")
  test(run("({ vcc v; (v(31) * 31) } * 7)"), "217")
  test(run("({ vcc n; (n(31) / 31) } / 7)"), "4")
  test(run("(31 / (-7))"), "-4")
  test(run("((-31) / 7)"), "-4")
  test(run("((-31) / (-7))"), "4")
  test(run("({ vcc q; (q(31) % 31) } % 7)"), "3")
  test(run("((-31) % 7)"), "-3")
  test(run("(7 == 7)"), "true")
  test(run("({ vcc m; (m(31) == 31) } == 7)"), "false")
  test(run("(7 < 7)"), "false")
  test(run("({ vcc g; (g(31) < 31) } < 7)"), "false")
  test(run("{if ({ vcc n; {if (n(true)) 31 else 7 } }) 7 else 31 }"), "7")
  test(run("{if (true) 7 else u }"), "7")
  test(run("{if (false) 7 else 31 }"), "31")
  test(run("((7, 31), (7, 31))"), "((7, 31), (7, 31))")
  test(run("({ vcc c; (c(7), 7) }, 31)"), "(7, 31)")
  test(run("(7, 31)._1"), "7")
  test(run("{ vcc l; l((7, 31))._1 }._2"), "31")
  test(run("(31 :: { vcc s; (7 :: s((7 :: Nil))) })"), "(31 :: (7 :: Nil))")
  test(run("(Nil :: Nil)"), "(Nil :: Nil)")
  test(run("Nil.isEmpty"), "true")
  test(run("{ vcc c; (7 :: c(Nil)) }.isEmpty"), "true")
  test(run("(7 :: Nil).isEmpty"), "false")
  test(run("{ vcc d; d((7 :: Nil)).head }.isEmpty"), "false")
  test(run("(7 :: Nil).head"), "7")
  test(run("{ vcc c; c((7 :: Nil)).head }.head"), "7")
  test(run("(31 :: (7 :: Nil)).tail"), "(7 :: Nil)")
  test(run("{ vcc j; j((31 :: (7 :: Nil))).tail }.tail"), "(7 :: Nil)")
  test(run("{ val u = 7; u }"), "7")
  test(run("{ val u = true; { val u = 31; u } }"), "31")
  test(run("{ val u = 7; { val c = u; c } }"), "7")
  test(run("(() => 7)()"), "7")
  test(run("((u, c) => (u + c))(7, 31)"), "38")
  test(run("((u) => ((c) => (u + c)))(7)(31)"), "38")
  test(run("{ val u = 7; { def u() = u; u } }"), "<function>")
  test(run("{ val u = 7; { def u() = u; u() } }"), "<function>")
  test(run("{ val c = 7; { def u() = c; u() } }"), "7")
  test(run("{ def d(r, g, k) = {if ((r < 2)) (r + 2) else (d((r + (-1)), (g + 4), (g + 2)) + (g + 3)) }; d(12, 13, 10) }"), "399")
  test(run("{ def v(f, m, z) = {if ((f < 3)) (m + 1) else (a((f + (-1))) + (z + 2)) }; def a(l) = {if ((l < 1)) (l + 3) else (v((l + (-1)), (l + 2), (l + 1)) + (l + 5)) }; a(14) }"), "168")
  test(run("{ def m(q, s, d) = {if ((s < 1)) (q + 4) else (l((q + 1), (s + 1), (s + (-2))) + (s + 5)) }; def n(y, h, j) = {if ((y < 3)) (y + 4) else (f((y + (-1)), (y + 1), (h + 3)) + (y + 1)) }; def l(z, u, a) = {if ((a < 1)) (z + 4) else (p((a + 2), (a + (-1))) + (a + 3)) }; def p(a, q) = {if ((q < 1)) (a + 3) else (n((q + (-2)), (a + 3), (a + 3)) + (q + 2)) }; def f(r, g, o) = {if ((r < 3)) (g + 5) else (m((o + 4), (r + (-1)), (o + 3)) + (g + 1)) }; f(14, 10, 12) }"), "102")
  test(run("(7.isInstanceOf[Int], true.isInstanceOf[Int], (7, 31).isInstanceOf[Int], Nil.isInstanceOf[Int], (7 :: Nil).isInstanceOf[Int], ((c) => c).isInstanceOf[Int], { vcc u; u.isInstanceOf[Int] }, 7.isInstanceOf[Boolean], false.isInstanceOf[Boolean], (7, 31).isInstanceOf[Boolean], Nil.isInstanceOf[Boolean], (7 :: Nil).isInstanceOf[Boolean], ((h) => h).isInstanceOf[Boolean], { vcc u; u.isInstanceOf[Boolean] }, 7.isInstanceOf[Tuple], false.isInstanceOf[Tuple], (7, 31).isInstanceOf[Tuple], Nil.isInstanceOf[Tuple], (7 :: Nil).isInstanceOf[Tuple], ((x) => x).isInstanceOf[Tuple], { vcc u; u.isInstanceOf[Tuple] }, 7.isInstanceOf[List], false.isInstanceOf[List], (7, 31).isInstanceOf[List], Nil.isInstanceOf[List], (7 :: Nil).isInstanceOf[List], ((h) => h).isInstanceOf[List], { vcc u; u.isInstanceOf[List] }, 7.isInstanceOf[Function], true.isInstanceOf[Function], (7, 31).isInstanceOf[Function], Nil.isInstanceOf[Function], (7 :: Nil).isInstanceOf[Function], ((s) => s).isInstanceOf[Function], { vcc u; u.isInstanceOf[Function] })"), "(true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, true, true)")
  test(run("({ vcc g; g(7) }.isInstanceOf[Int], { vcc m; m(true) }.isInstanceOf[Int])"), "(true, false)")
  test(run("{ vcc n; (true + n(7)) }"), "7")
  test(run("{ vcc k; (k(7) + k(31)) }"), "7")
  test(run("{ vcc s; 7(s(31)) }"), "31")
  test(run("{ vcc d; ((u, c) => (u + c))(d(7), d(31)) }"), "7")
  test(run("{ try 7 catch 31 }"), "7")
  test(run("{ try { throw 7 } catch ((j) => j) }"), "7")
  test(run("{ try { throw { throw 7 } } catch ((n) => n) }"), "7")
  test(run("{ try { throw 7 } catch { vcc l; l(((p) => p))(31) } }"), "7")
  test(run("{ try { try { throw 7 } catch ((k) => k) } catch ((u) => (u + u)) }"), "7")
  test(run("{ try ({ throw 7 } + { throw 31 }) catch ((g) => g) }"), "7")
  test(run("{ try { try { throw 31 } catch { throw 7 } } catch ((p) => p) }"), "7")
  test(run("{ val u = (() => { throw 7 }); { try u() catch ((p) => p) } }"), "7")
  testExc(run("{ val u = 7; c }"), "")
  testExc(run("(7 + false)"), "")
  testExc(run("(7 * false)"), "")
  testExc(run("(7 / true)"), "")
  testExc(run("(7 / ((x) => x)(0))"), "")
  testExc(run("(7 % true)"), "")
  testExc(run("(7 % ((f) => f)(0))"), "")
  testExc(run("(7 == false)"), "")
  testExc(run("(7 < false)"), "")
  testExc(run("{if (7) 7 else 31 }"), "")
  testExc(run("(31 :: (7 :: Nil))._1"), "")
  testExc(run("(7, 31)._3"), "")
  testExc(run("(31 :: 7)"), "")
  testExc(run("(7, 31).isEmpty"), "")
  testExc(run("(7, 31).head"), "")
  testExc(run("(7, 31).tail"), "")
  testExc(run("((h) => h)(Nil).head"), "")
  testExc(run("((w) => w)(Nil).tail"), "")
  testExc(run("{ val c = (() => u); { val u = 7; c() } }"), "")
  testExc(run("7()"), "")
  testExc(run("((n) => n)()"), "")
  testExc(run("((a) => a)(7, 31)"), "")
  testExc(run("({ try 7 catch ((b) => b) } + { throw 31 })"), "")

  test(run("""
  def fib(x) = {
    if (x == 0)
      return 0
    else if (x == 1)
      return 1
    else
      return fib(x -1) + fib(x - 2)
  };
  fib(9)
  """), "34")
  test(run("""
  def fill(a, n) =
    if (n == 0)
      return Nil
    else
      return a :: fill(a, n - 1);

  def map(l, f) =
    if (l.isEmpty)
      return Nil
    else
      return f(l.head) :: map(l.tail, f);

  def filter(l, p) =
    if (l.isEmpty)
      return Nil
    else if (p(l.head))
      return l.head :: filter(l.tail, p)
    else
      return filter(l.tail, p);

  def foldLeft(a, l, f) =
    def aux(i, r) =
      if (r.isEmpty)
        return i
      else
        return aux(f(i, r.head), r.tail);
    return aux(a, l);

  val l = fill(1, 10);
  val l = foldLeft((0, Nil), l, (a, b) => (a._1 + b, a._1 + b :: a._2))._2;
  val l = filter(l, x => x % 2 == 0);
  val l = map(l, x => x * x);
  foldLeft(0, l, (a, b) => a + b)
  """), "220")
  test(run("""
  def isPrime(n) = {
    if (!n.isInstanceOf[Int] || n <= 1)
      throw n
    else
      def aux(m) = {
        if (n == m)
          return true
        else if (n % m == 0)
          return false
        else
          return aux(m + 1)
      };
      return aux(2)
  };
  def factorize(n) = {
    if (!n.isInstanceOf[Int] || n <= 0)
      throw n
    else if (isPrime(n))
      return n :: Nil
    else
      def aux(m) = {
        if (n % m == 0)
          return m :: factorize(n / m)
        else
          return aux(m + 1)
      };
      return aux(2)
  };
  factorize(2940)
  """), "(2 :: (2 :: (3 :: (5 :: (7 :: (7 :: Nil))))))")
  test(run("""
  def interp(e) = {
    if(e.isInstanceOf[Int])
      return e
    else if(!e.isInstanceOf[List] || e.isEmpty)
      throw e
    else if(e.head)
      return interp_plus(e.tail)
    else
      return interp_minus(e.tail)
  };
  def interp_plus(e) = {
    if(!e.isInstanceOf[List] || e.isEmpty || e.tail.isEmpty || e.tail.tail.nonEmpty)
      throw e
    else
      val v1 = interp(e.head);
      val v2 = interp(e.tail.head);
      v1 + v2
  };
  def interp_minus(e) = {
    if(!e.isInstanceOf[List] || e.isEmpty || e.tail.isEmpty || e.tail.tail.nonEmpty)
      throw e
    else
      val v1 = interp(e.head);
      val v2 = interp(e.tail.head);
      v1 - v2
  };
  def f(x) = try interp(x) catch (e=>false);
  (f(42), f(true::(false::3::1::Nil)::5::Nil), f(false::2::Nil), f(true::0::true::Nil), f(false::2::3::4::Nil))
  """), "(42, 7, false, false, false)")
  test(run("""
  def break(x) = throw (Nil, x);
  def loop(f, init) = {
    def aux(n) = aux(f(n));
    try {
      aux(init)
    } catch {
      x =>
        if (x.isInstanceOf[Tuple] && x._1.isInstanceOf[List])
          x._2
        else
          throw x
    }
  };
  def f(p) =
    if (p._1 <= 0) break(p)
    else (p._1 - 1, p._1 + p._2);
  val sum = n => loop(f, (n, 0))._2;
  sum(11)
  """), "66")
  test(run("""
  def Some(x) = (true, x);
  val None = (false, 0);
  def get(opt) =
    if (opt.isInstanceOf[Tuple] && opt._1.isInstanceOf[Boolean] && opt._1)
      opt._2
    else
      throw opt;
  def flatMap(opt, f) =
    if (opt.isInstanceOf[Tuple] && opt._1.isInstanceOf[Boolean])
      if (opt._1)
        f(opt._2)
      else
        opt
    else
      throw opt;
  def lift(f) =
    x =>
      try {
        Some(f(x))
      } catch {
        x => None
      };
  def div100(x) =
    if (x.isInstanceOf[Int] && x != 0)
      100 / x
    else
      throw x;
  val safeDiv = lift(div100);
  get(flatMap(safeDiv(5), safeDiv)) + try {
    get(flatMap(safeDiv(0), safeDiv))
  } catch {
    x => 0
  }
  """), "5")
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

  mergeSort(3 :: 1 :: 6 :: 4 :: 2 :: 5 :: Nil)
  """), "(1 :: (2 :: (3 :: (4 :: (5 :: (6 :: Nil))))))")
  test(run("""
  def len(l) = if (l.isEmpty) 0 else 1 + len(l.tail);

  val yin = {
    val k = (vcc x; x);
    if (k.isInstanceOf[Function])
      (k, Nil)
    else if (len(k._2) < 10)
      (k._1, 1 :: k._2)
    else
      ((x) => x, k._2)
  };
  val yang = {
    val k = (vcc x; x);
    if (k.isInstanceOf[Function])
      (k, yin._2)
    else
      (k._1, 2 :: k._2)
  };
  val r = yin._1(yang);
  yin._2
  """), "(2 :: (2 :: (2 :: (2 :: (1 :: (2 :: (2 :: (2 :: (1 :: (2 :: (2 :: (1 :: (2 :: (1 :: Nil))))))))))))))")

  /* Write your own tests */
}
