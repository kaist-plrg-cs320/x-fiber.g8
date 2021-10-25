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

  // Fall 2020
  test(run("if (false || (1 == 2)) 42 else 84"), "84")
  testExc(run("Nil.head"), "")
  testExc(run("(2, 3)._4"), "")
  testExc(run("(1, false)._9"), "")
  testExc(run("(a => a._1 * a._2 / a._3)(1, 7)"), "")
  testExc(run("1 :: 2 + 3"), "")
  test(run("3 + 7 * 2"), "17")
  test(run("42 :: 2 - 4 :: 23 > 11 && !false :: Nil"), "(42 :: (-2 :: (true :: Nil)))")
  test(run("(17, 42, 38)._2 / 3 == 14 || false :: false :: Nil"), "(true :: (false :: Nil))")
  test(run("if (!!true) 3 > 2 else (4 || true)"), "true")
  testExc(run("if (!!!true) 3 > 2 else (4 || true)"), "")
  testExc(run("if (false || true) (4 || true) else 4 * 10 + 2"), "")
  test(run("if (1 < 17 && 3 == 4) (4 || true) else 4 * 10 + 2"), "42")
  testExc(run("(123 :: 4 && true :: Nil).head"), "")
  testExc(run("((a, b) => a)(1, 1 || true)"), "")
  testExc(run("""
    val foo = (a => 1);
    foo((1, 2, 3)._4)
  """), "")
  testExc(run("""
    def f(x, y) = if (x < 1) 0 else x + f(x - 1);
    f(10, Nil.tail)
  """), "")
  test(run("""
    val x = 4;
    val f = (x => x * x);
    f(12)
  """), "144")
  test(run("""
    val x = 6;
    def f(y) = if (y == 0) 0 else x + f(y - 1);
    f(10)
  """), "60")
  test(run("""
    val (a, b) = (42, true);
    val f = (b => if (b) a else 7);
    f(false)
  """), "7")
  test(run("""
    val a = 42;
    val f = (x => a);
    f(12)
  """), "42")
  test(run("""
    def f(x) = if (x % 2 == 1) f(x - 1) else x + g(x);
    def g(x) = if (x == 0) 0 else x * x + f(x - 2);
    f(11)
  """), "250")
  test(run("""
    def f(x) = if (x == 0) 0 else (if (x % 3 == 2) h(x - 1) else x + g(x - 1));
    def g(x) = if (x == 0) 0 else (if (x % 3 == 1) f(x - 1) else x + h(x - 1));
    def h(x) = if (x == 0) 0 else (if (x % 3 == 0) g(x - 1) else x + f(x - 1));
    f(9)
  """), "45")
  test(run("vcc a; a"), "<continuation>")
  test(run("if(vcc k; k(true) + 1)  1 else 0"),"1")
  test(run("vcc done; {  vcc esc; done((1 + { vcc k; esc(k) }))  }(3)"), "4")
  test(run("vcc done; {  vcc esc; done((vcc a;esc(a), vcc b; esc(b), vcc c; esc(c)))  }(7)"), "(7, 7, 7)")
  test(run("{ vcc esc; x => 1 + vcc k; esc(k) }(3)"), "4")
  test(run("(x => return (1 + 2 + (return x * x))) (3)"),"9")
  test(run("val x = {vcc esc; esc(esc)}; x( f => (a => a + 1) )(0)"), "1")
  test(run("def get_head(L) = L.head; val L = {vcc a; get_head :: vcc b; get_head :: vcc c; a(b::c::Nil)}; L.head(1::Nil)"),"1")
  test(run("val f = {x => vcc k; if(x.isInstanceOf[Function]) x(k) else k(x)}; f(0)"), "0")
  test(run("val f = {x => vcc k; if(x.isInstanceOf[Function]) x(k) else k(x)}; f(f)"), "<continuation>")
  test(run("try throw throw 0 catch (x => x)"), "0")
  test(run("try throw (x, y) => x + y catch (f => f(3, 4))"), "7")
  test(run("val neg = (x => -x); (try throw 42 catch neg) + (try throw -42 catch neg)"), "0")
  test(run("def neg(x) = -x; (try throw 42 catch neg) + (try throw -42 catch neg)"), "0")
  test(run("try try throw 0 catch (x => throw (x + 1)) catch (x => x * 2)"),"2")
  test(run("try throw try throw 0 catch (x => x + 1) catch (x => x * 2)"),"2")
  test(run("try throw try 0 catch (x => x + 1) catch (x => x * 2)"),"0")
  test(run("try try throw 0 catch (x => x + 1) catch (x => x * 2)"),"1")
  test(run("try try 0 catch (x => x + 1) catch (x => x * 2)"),"0")
  test(run("try 0 catch (1/0 + 1(2) + x + true + (1::2))"), "0")
  test(run("vcc done; try { vcc esc; try {vcc k; esc(k)}(true) catch {vcc k; esc(k)} }(x => throw !x) catch done"), "true")
  testExc(run("if (vcc c; try throw true catch (x => try c(x) catch (y => false))) throw 0 else 0"),"")
  test(run("vcc done; done(1) - done(0)"), "1")
  test(run("vcc done; done(2) == done(0)"), "2")
  test(run("vcc done; done(3) :: done(0)"), "3")
  test(run("vcc done; {done(4)}(done(0))"), "4")
  test(run("vcc done; def f(x) = 2 * x; f(1, done(5))"), "5")
  test(run("try (throw 6) % (throw 0) catch (n => n)"), "6")
  test(run("try (throw 7) <= (throw 0) catch (n => n)"), "7")
  test(run("try ((throw 8),(throw 0)) catch (n => n)"), "8")
  test(run("try {(x,y)=>x+y}(throw 9, throw 0) catch (n => n)"), "9")
  test(run("try vcc k; k(1, throw 10) catch (n => n)"), "10")
  test(run("try (true + throw 1) catch (x => x)"), "1")
  test(run("try 1(throw 2) catch (x => x)"), "2")
  test(run("try (x => x)(1, throw 2) catch (x => x)"), "2")
  test(run("vcc x; throw x(1)"), "1")
  testExc(run("vcc k; k(0, 1)"), "")
  testExc(run("vcc k; k()"), "")
  testExc(run("try throw 1 catch 2"), "")
  testExc(run("try throw 1 catch ((x, y) => x)"), "")
  testExc(run("try throw 1 catch (() => false)"), "")
  testExc(run("try if(0) true else false catch (x => x)"), "")
  testExc(run("try throw(true(0)) catch (x => x)"),"")
  testExc(run("{vcc done; val x = 0; done(0)} + x"), "")
  testExc(run("try def f(x) = x; throw(0) catch f"),"")
  testExc(run("val f = try (x => throw x) catch (x => x); f(0)"), "")
  test(run("""
  def fib(x) = {
      if (x == 0)
          return 0
      else if (x == 1)
          return 1
      else
          return fib(x -1) + fib(x - 2)
  };
  fib(10)
  """), "55")
  test(run("""
  def match(L) = {
      if(L.isEmpty)
          throw -1
      else
          (L.head, L.tail)
  };
  try
      val t = match(1::Nil)._2;
      match(t)
  catch (x => x)
  """), "-1")
  test(run("""
  val MAX = 10;
  def f(a, b) = {
      if (a == 0)
          return true
      else if (b == 0)
          return f(a-1, MAX)
      else if (b*(a/b) +  a%b != a)
          throw false
      else
          return f(a, b-1)
  };
  f(MAX, MAX)
  """), "true")
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
  (f(42), f(true::(false::2::1::Nil)::3::Nil), f(false::1::Nil), f(true::1::true::Nil), f(false::1::1::1::Nil))
  """), "(42, 4, false, false, false)")
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
    sum(10)
  """), "55")
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
    get(flatMap(safeDiv(10), safeDiv)) + try {
      get(flatMap(safeDiv(0), safeDiv))
    } catch {
      x => 0
    }
  """), "10")
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

    mergeSort(2 :: 5 :: 0 :: 3 :: 4 :: 1 :: Nil)
  """), "(0 :: (1 :: (2 :: (3 :: (4 :: (5 :: Nil))))))")
  test(run("""
    def len(l) = if (l.isEmpty) 0 else 1 + len(l.tail);

    val yin = {
      val k = (vcc x; x);
      if (k.isInstanceOf[Function])
        (k, Nil)
      else if (len(k._2) < 10)
        (k._1, 2 :: k._2)
      else
        ((x) => x, k._2)
    };
    val yang = {
      val k = (vcc x; x);
      if (k.isInstanceOf[Function])
        (k, yin._2)
      else
        (k._1, 1 :: k._2)
    };
    val r = yin._1(yang);
    yin._2
  """), "(1 :: (1 :: (1 :: (1 :: (2 :: (1 :: (1 :: (1 :: (2 :: (1 :: (1 :: (2 :: (1 :: (2 :: Nil))))))))))))))")

  // Spring 2021
  test(run("((1 / 2) + (2 % 3))"), "2")
  test(run("((((1 * 2) + (1 * 1)) * 2) % (((3 + 2) + (-1 % 2)) + ((3 % 2) + (3 % 3))))"), "1")
  test(run("((3 / 2) + (2 % 3))"), "3")
  test(run("((0 / 1) + (((0 % 3) + (1 / 1)) * 1))"), "1")
  test(run("((((2 * 2) + (2 * 1)) + 1) % (2 * (2 + (2 * 2))))"), "7")
  test(run("{if (((3 * 2) < (2 * 2))) {if ({if (true) false else true }) (2 + 3) else (3 % 2) } else {if ((3 < 0)) (2 % 3) else 2 } }"), "2")
  test(run("(((2 % 3) + {if (true) 3 else 2 }) + ((0 % 3) + (2 + 3)))"), "10")
  test(run("{if ({if ((-1 < 3)) {if (true) false else false } else {if (true) false else true } }) 2 else {if ({if (true) false else false }) (2 / 3) else (1 * 2) } }"), "2")
  test(run("(((2 * 3) + {if (true) 0 else 3 }) + ({if (false) 1 else 1 } * (3 * 2)))"), "12")
  test(run("(((3 + 1) / (0 + 1)) * {if ((2 < 2)) {if (false) 0 else 1 } else {if (true) 1 else 2 } })"), "4")
  test(run("((0, 3)._2 * ((3, 3, 3)._1 + (1 / 3)))"), "9")
  test(run("((2, 3)._1, (1, 3, 0, 2), (-1, 2, 2), (3 + 2))._1"), "2")
  test(run("((2, 3, 3)._2 % (2, 2, 3)._1)"), "1")
  test(run("(((2 * 0) + 2) + ((3, -1)._1 % (3, 2, -1, 1)._3))"), "2")
  test(run("(3, (3, 3))._2._1"), "3")
  test(run("((3 * 3) :: (1 :: Nil).tail).head"), "9")
  test(run("(0 :: (3 :: Nil)).tail.head"), "3")
  test(run("((2 + 1) :: (1 :: Nil).tail).head"), "3")
  test(run("(3 :: (-1 :: Nil)).tail.head"), "-1")
  test(run("((3 :: Nil).head :: (1 :: Nil).tail).head"), "3")
  test(run("(3 + ({ val t = 3; t } / (3 + -1)))"), "4")
  test(run("((2 * { val u = 1; u }) * (2 / (3 % 2)))"), "4")
  test(run("(((2 / -1) + (1 / 1)) + ({ val r = 2; r } + (1 * 0)))"), "1")
  test(run("{ val w = ((3 % 1) + (3 * 3)); ((1 % w) * (2 + w)) }"), "11")
  test(run("{ val z = ((1 * 2) + (2 + 0)); { val u = z; (u + u) } }"), "8")
  test(run("(((2 * 2) + ((t, r) => t)(2, 3)) % ((2 * 2) * ((s) => s)(1)))"), "2")
  test(run("((u, q, y) => (u / 1))(((0 * 2) + (1 * 3)), 2, ((-1 * 3) * ((q, z) => 1)(3, 3)))"), "3")
  test(run("((t) => ((t) => t)(t))(((q, x) => 1)(((x) => x)(2), (1 + 3)))"), "1")
  test(run("((t, z, s) => ((w, u, r) => r)(z, s, t))(((w) => w)((3 + 3)), 3, (() => 2)())"), "6")
  test(run("(((v, t) => 3)((3 + 2), (1 / 1)) * ((s, q) => s)((3 + 2), (3 + 3)))"), "15")
  test(run("((y, v) => ((x) => ((s) => 3)))(3, 3)(((u, w) => u)(3, 1))(((2 + 1) * (2 + 3)))"), "3")
  test(run("(((1 + 3) + (3 * 1)) + (((w, r) => 2)(3, 1) + ((z, t) => 3)(3, 3)))"), "12")
  test(run("{ def t(q, x) = ((q) => ((y, v) => 3))(x); def v(w) = w(1, 3); def z(r, y) = ((y, x) => y)(y, r); v(z) }"), "3")
  test(run("{ def w(u) = ((s, u) => u)(0, u); def t(v) = v(2); t(w) }"), "2")
  test(run("{ def w(s, r) = r(s, s); def y(q) = { def t() = q; def w(q, y) = y; q }; def u(w, s) = { def v(z, q) = s; def w(y) = 1; 1 }; w(2, u) }"), "1")
  test(run("{ def w(v) = v(1, 0); def q(t, w) = (t + w); w(q) }"), "1")
  test(run("{ def q(r) = r(2); def x(s, z) = (z + 1); def v(t) = (1 % t); q(v) }"), "1")
  test(run("{ def v(u, s) = ((v, q) => u)(u, s); def w(u, q) = u(3); def s(u) = (u * 3); w(s, 3) }"), "9")
  test(run("{ def x(y, z) = 3; def s(r) = r(3, -1); def t() = ((v) => (() => v))(1); s(x) }"), "3")
  test(run("{ def x(q) = -1; def r(v, y) = ((q) => y)(-1); def q(y) = y(0); q(x) }"), "-1")
  test(run("{ vcc z; ((z(2) % z(3)) + { vcc z; (3 % 1) }) }"), "2")
  test(run("((((u, t) => 3)({ vcc y; 2 }, (-1 % -1)) / { vcc v; { vcc q; 1 } }) / ({ vcc v; v(3) } / { vcc t; (2 + 1) }))"), "3")
  test(run("(({ vcc s; s(2) } + { vcc s; ((u, q, s) => s)(2, 2, 2) }) * ({ vcc t; (3 / 3) } + ((z) => ((q, w, z) => q))(2)(((z) => z)(2), 1, (3 + 2))))"), "12")
  test(run("{ vcc x; ({ vcc u; (-1 * 3) } + (x(3) * (1 + 2))) }"), "3")
  test(run("({ vcc y; y(((r) => r)) }(((s, w) => ((y) => s))(3, 0)((1 % -1))) * { vcc u; ((2 * 3) / (1 % 3)) })"), "18")
  test(run("({ vcc t; ((3 % 1) + t(1)) } * (((2 + 2) * (() => 2)()) * ((-1 * 2) / (2 / 2))))"), "-16")
  test(run("{ vcc y; y(y(((t) => t))) }((((w, v) => ((x, t) => 1))(2, 1)((2 + 3), (3 % 2)) + { vcc q; q(2) }))"), "3")
  test(run("{ vcc y; ((y(1) * { vcc r; 1 }) * ({ vcc t; 2 } * (3 + 3))) }"), "1")
  test(run("{ vcc z; (((r, x) => 1)((2 / 2), z(3)) / { vcc q; (-1 + 0) }) }"), "3")
  test(run("{ vcc s; s((s(3) / 1)) }"), "3")
  test(run("({ vcc y; y((3 + -1)) } + { vcc z; (z(3) / (2 + 1)) })"), "5")
  test(run("(({ vcc y; y(3) } * ((1 / 2) + (2 % 3))) * ((y, u, t) => ((q) => t))({ vcc t; 2 }, { vcc s; 2 }, ((u, t) => 3)(2, 2))(((z, x, v) => z)((2 * 3), ((q) => q)(3), (2 * 3))))"), "18")
  test(run("{ vcc u; u((u(3) * u(3))) }"), "3")
  test(run("(1 / { try { throw 2 } catch ((z, r, u) => ((u) => -1))(2, 1, 1) })"), "-1")
  test(run("(((2 + 3) / (1 % 2)) * ((y) => y)({ try { throw 3 } catch ((v) => v) }))"), "15")
  test(run("(((u) => ((z, t, u) => 3))(3)((1 % 2), (2 * 2), (2 % 3)) + ((z, y) => y)(((w, r) => 1)(2, 1), { try { throw 2 } catch ((y) => 2) }))"), "5")
  test(run("({ try { throw { throw 1 } } catch ((t, s, v) => ((r) => s))(3, 3, 3) } % (((u, q) => q)(2, 2) / { try 1 catch ((v) => v) }))"), "1")
  test(run("((z, t) => { try z catch ((z) => z) })(({ try 2 catch ((y) => 0) } + (-1 / 1)), { try { throw 3 } catch { try ((r) => r) catch ((x) => ((w) => w)) } })"), "1")
  test(run("({ try ({ throw 2 } % 3) catch { try ((q) => q) catch ((w) => ((y) => y)) } } * ((t, s) => ((u, s, r) => 2))(0, -1)(((z, s) => s)(((v, z, s) => v), 3), { try 1 catch ((z) => z) }, (1 + -1)))"), "4")
  test(run("{ try { try ({ throw 3 } % { throw 1 }) catch { try ((u) => 1) catch ((y) => ((z) => y)) } } catch ((w) => (w / w)) }"), "1")
  test(run("((r, u) => ((s, t) => 1))(3, (1 % 3))({ try { throw -1 } catch ((s) => ((q, s) => 1)) }((() => 3)(), (2 * 1)), { try (-1 % { throw 3 }) catch { try ((z) => z) catch ((t) => ((v) => v)) } })"), "1")
  test(run("{ try { throw 1 } catch ((t, u, w) => ((z, w) => ((t) => t)))(-1, 3, 1)(1, (2 + 2)) }"), "1")
  test(run("((w, z) => (() => z)())({ try { throw 1 } catch ((q) => q) }, { try { throw 1 } catch ((v) => v) })"), "1")
  test(run("{ try ({ throw { throw 1 } } / (-1 * 3)) catch ((q) => ((u, z) => z)(((x) => 3), q)) }"), "1")
  test(run("{ try ({ throw 1 } / ((v, t) => 2)(3, 3)) catch { try { throw (() => 2) } catch ((t) => ((r) => r)) } }"), "1")
  test(run("{ try (({ throw 1 } % 2) * (2 + 2)) catch ((z) => ((u, w) => 2)(z, z)) }"), "2")
  test(run("{if ((true, ((t, r) => true), 1)._2((1, (((3 :: Nil), true), (() => true), false), ((y, v) => y), true)._3, (true :: Nil))) { try (2, ((z) => (true :: Nil)), { throw (2 :: Nil) })._3 catch { try ((x) => 2) catch ((q) => ((r) => q)) } } else { def u(q) = (false, 3, 3)._2; def w(x, u) = ((1 :: Nil) :: Nil).isEmpty; def x(s, u) = s._1; { val q = true; 1 } } }"), "2")
  test(run("{if ({ try { throw 3 } catch ((z) => ((v, w) => false)) }(((((2, 2), (-1, (3 :: Nil), ((1 :: Nil) :: Nil)), 1, (2 :: Nil)), (((t, q) => false) :: Nil), 3), ((v, x, q) => (1 :: Nil)))._1, ((z, s) => z)(1, (3, (true, 2, 2, ((q) => true)), (((true :: Nil) :: Nil) :: Nil))))) { try ((x) => 0) catch ((q) => ((q) => 3)) }(3) else (() => 1)() }"), "1")
  test(run("((((r, u) => u) :: Nil) :: Nil).head.head({ try { val z = ({ throw 1 }, (1 :: Nil), 3, { throw 2 }); ((true, (Nil, 3, 1, -1), 1) :: Nil) } catch { vcc z; ((x) => ((false, ((x :: Nil), 1, x, x), x) :: Nil)) } }, { val u = 3; (u :: Nil) }.head)"), "3")
  test(run("{ try {if ({ throw 2 }) ((r, s) => 3) else ((u, r) => -1) } catch {if (true) ((x) => ((z, r) => x)) else ((s) => ((q, w) => s)) } }({ val v = { try 1 catch ((x) => x) }; (((q, w, u) => true) :: Nil).isEmpty }, {if ((1 :: Nil).isEmpty) (false :: Nil).isEmpty else ((q, z) => false)(false, 3) })"), "2")
  test(run("{if ({ try { val x = 3; { throw (x, false) } } catch (((v) => false) :: Nil).head }) ((r) => r)(1) else ((w) => ((true, ((y) => false)), (((false, (((false :: Nil), (w, w), true, Nil) :: Nil), w), ((x) => ((2, x, w), false, x, (((((r) => true) :: Nil), x, w), x, (((-1 :: Nil), true), w, w))))) :: Nil), w, w))(1)._3 }"), "1")
  test(run("{if ({ vcc y; ((y, w, z) => true) }({ try { throw ((3 :: Nil) :: Nil) } catch ((r) => (true :: Nil)) }, (() => 2)(), { val z = Nil; (((2, (2, 1, true, (((1 :: Nil), (Nil :: Nil)), 2)), 3), true, 2), ((t, u) => ((y) => (1, (((((u :: Nil), y, z, y), y), u) :: Nil))))) })) (3 :: Nil).head else { def x(y, z) = { def t(w) = 2; true }; def y(r, u) = {if (r) r else r }; 0 } }"), "3")
  test(run("{ try { throw { try (((((y, u) => true) :: Nil), ({ throw ((t, z) => -1) } :: Nil), { throw ((v, y) => 3) }, 2), { throw ((z, t) => 2) }, (1 :: Nil), (3, false)) catch ((s) => ({ throw (((((t, u) => true) :: Nil), (false :: Nil), false, 0), 3, (2 :: Nil), (2, true)) }, 3, (1 :: Nil), { throw (((((z, v) => true) :: Nil), Nil, true, 3), 3, (2 :: Nil), (-1, false)) })) } } catch { vcc w; ((s, w) => ((r) => 1)) }({ val u = (-1, true, (3, (true :: Nil))); (2, ((((false :: Nil) :: Nil) :: Nil), true), false, (((1, true, 1), (-1 :: Nil), (3 :: Nil)), true, ((false, false), 3))) }, {if (true) false else false }) }"), "1")
  test(run("{ val v = ((u, q, r) => r)((1 :: (3 :: Nil)), (((y, x) => y), ((true :: Nil) :: Nil), ((-1 :: Nil) :: Nil))._3, (3, 3, ((3, (2, ((true :: Nil), (1, ((t) => 1), 2), (((false, (0 :: Nil)) :: Nil) :: Nil)), false), 2, ((s) => ((Nil :: Nil) :: Nil))) :: Nil), (false, false, -1))._2); {if ({ try { throw true } catch ((y) => y) }) {if (true) v else v } else v } }"), "3")
  test(run("{ try {if (((r, x) => { throw (3, ((3 :: Nil) :: Nil), ((s, q) => (q :: Nil))) })(((t, y) => Nil), { throw (2, ((2 :: Nil) :: Nil), ((t, s) => Nil)) })) {if (true) 3 else 2 } else (({ throw (1, Nil, ((s, v) => Nil)) }, (() => { throw (3, ((3 :: Nil) :: Nil), ((x, v) => Nil)) }), 2, { throw (1, (Nil :: Nil), ((y, u) => Nil)) }), 2)._2 } catch { val x = ((r) => (1, (Nil :: Nil), 2))(((3 :: Nil), 2, 3)); ((r) => 2) } }"), "2")
  test(run("{ try { val s = (({ throw 3 }, { throw 2 }, ((r, x) => 3), 2) :: ((2, 0, ((t, x) => 1), -1) :: Nil)); (2, ((({ throw -1 }, 1, true), { throw 2 }, (((u) => (Nil, ((x, w) => ((2 :: Nil), w, u)))), { throw 2 }, false)) :: Nil), { throw 1 }, 2)._4 } catch (((s) => ((r) => s)) :: Nil).head((((3, true, false) :: Nil), 1, 3)._3) }"), "3")
  test(run("{ try { throw (false :: Nil) } catch ((x) => ((r, x, z) => 3)) }({ vcc y; ((2, true, 2, 0) :: Nil).head }, {if (true) (false :: Nil) else (true :: Nil) }.head, (-1, { val v = false; 2 }))"), "3")
  test(run("{ try (2, ((v) => 0), ((({ throw true } :: Nil) :: Nil), 2, ({ throw true }, true, { throw true }), (((2, false, ({ throw false }, Nil)), true, 1, ((2 :: Nil) :: Nil)) :: Nil))) catch ((x) => (1, ((v) => 3), ((((true, Nil, ((false, 3, (x :: Nil)) :: Nil), (1 :: Nil)) :: Nil) :: Nil), 3, ((2, x, 2, 1), x, 3), (((1, x, (3, (Nil :: Nil))), x, 3, ((1 :: Nil) :: Nil)) :: Nil)))) }._2((((u, q) => false) :: Nil).head(((2, false) :: Nil).head, { try ((v) => ((q, s, u) => ({ throw false }, ((y, z, t) => (((Nil :: Nil), 2, ((z, 2, z), t), t), 1, ((s, t) => (s, ((s) => 2), (s, (s :: Nil), ((s :: Nil) :: Nil)))))), ((2, ((false, 3, (3, { throw false }, 3), 2), true)), ({ throw true } :: Nil), 1), 3))) catch ((y) => ((u) => ((y, v, z) => (-1, ((w, q, r) => ((((1 :: Nil) :: Nil), 3, ((true, 1, q), true), r), 2, ((s, r) => (s, ((u) => s), (2, (3 :: Nil), Nil))))), ((3, ((false, 3, (3, false, 3), 3), true)), (2 :: Nil), 2), 3)))) }))"), "3")
  test(run("{if (((2, 2), 2, ((t, u, q) => q))._3((1 :: Nil).isEmpty, { try ((2 :: Nil), { throw Nil }) catch ((q) => ((0 :: Nil), 3)) }, ((x, w) => false)((2 :: Nil), Nil))) { def w(z, x) = Nil; def z(t, w) = t; def s(w, q) = 1; 3 } else { def w(s, q) = (1, (true :: Nil), (s, (s, (((s, u, r) => q), q)), (q :: Nil)), ((false :: Nil), s, true))._2; def s(w, v) = { val t = ((() => (1, w, 2)) :: Nil); (2, 3) }; ((3, ((0 :: Nil) :: Nil)), 2, 0, 3)._4 } }"), "3")
  test(run("{if ({ try ((w, t, v) => true) catch ((x) => ((r, x, v) => true)) }({if (false) 2 else 2 }, { try 3 catch ((y) => 3) }, { def y(z, u) = ((u, (z, ((Nil, z, ((false :: Nil), u, z, 0)), z, (z, (z :: Nil), (z :: Nil)), 1), false), Nil, u), false); (((s) => (1, true, s)), 2, 0) })) { try { val w = (({ throw 2 }, (((3 :: Nil), (3 :: Nil), { throw 0 }, true), 1)) :: Nil); 0 } catch ((w, r) => ((z) => 2))((2 :: Nil), (3, 3, (1, (1, ((q) => ((q :: Nil) :: Nil))), true, (-1 :: Nil)), 2)) } else { vcc q; ((w, x) => 2) }((0 :: Nil).head, ((z, u) => z)(0, (2, (true, -1), Nil))) }"), "2")
  testExc(run("(3, 2)._4"), "")
  testExc(run("(2, true)._9"), "")
  testExc(run("(a => a._1 * a._2 / a._3)(2, 6)"), "")
  testExc(run("2 :: 4 + 8"), "")
  testExc(run("if (!!!true) 4 > 5 else (3 || true)"), "")
  testExc(run("if (false || true) (6 || true) else 2 * 13 + 1"), "")
  testExc(run("(321 :: 42 && false :: Nil).head"), "")
  testExc(run("((a, c) => a)(3, 3 || true)"), "")
  testExc(run("""
    val foo = (b => 1);
    foo((2, 3, 5)._4)
  """), "")
  testExc(run("""
    def g(x, y) = if (x < 1) 0 else x + f(x - 1);
    g(100, Nil.tail)
  """), "")
  testExc(run("if (vcc d; try throw true catch (x => try d(x) catch (y => false))) throw 1 else 1"),"")
  testExc(run("vcc k; k(1, 0)"), "")
  testExc(run("vcc g; g()"), "")
  testExc(run("try throw 3 catch 0"), "")
  testExc(run("try throw 4 catch ((c, z) => z)"), "")
  testExc(run("try throw 5 catch (() => 1234)"), "")
  testExc(run("try if(10) false else true catch (x => x)"), "")
  testExc(run("try throw(false(20)) catch (y => y)"),"")
  testExc(run("{vcc d; val y = 1; d(0)} + y"), "")
  testExc(run("try def f(x) = x + x; throw(23) catch f"),"")
  testExc(run("val g = try (x => throw x) catch (x => x); g(8)"), "")
  testExc(run("(vcc x; x)(10)"), "")
  testExc(run("val c = (vcc x; x); (f => f(10))(c)"), "")
  test(run("(true, 2, 2, Nil)._4.isEmpty"), "true")
  test(run("(false :: (2, Nil)._2)"), "(false :: Nil)")
  test(run("(1 % -1)"), "0")
  test(run("{ val r = 1; (r :: Nil).head }"), "1")
  test(run("{ def z() = 1; { val z = 1; ((t, q) => z) }(true, false) }"), "1")
  test(run("{ val z = 1; { def z() = 2; z } }"), "<function>")
  test(run("(Nil :: Nil)"), "(Nil :: Nil)")
  test(run("Nil.isInstanceOf[Int]"), "false")
  testExc(run("-1 / 0"), "")
  testExc(run("true / false"), "")
  testExc(run("false.isEmpty"), "")
  testExc(run("(() => 1)(Nil, false)"), "")
  testExc(run("(2 % (1 / 2))"), "")
  testExc(run("(Nil == 0)"), "")
  testExc(run("((z, w, x) => w)(false, ((x, v) => z)(3, 3), false)"), "")
  testExc(run("(2 % 0)"), "")
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
  def match(L) = {
    if(L.isEmpty)
      throw 0
    else
      (L.head, L.tail)
  };
  try
    val t = match(2 :: Nil)._2;
    match(t)
  catch (x => x)
  """), "0")
  test(run("""
  val m = 9;
  def f(a, b) = {
    if (a == 0)
      return true
    else if (b == 0)
      return f(a - 1, m)
    else if (b * (a / b) + a % b != a)
      throw false
    else
      return f(a, b - 1)
  };
  f(m, m)
  """), "true")
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
