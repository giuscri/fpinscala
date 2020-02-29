package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight(Cons(() => z, () => Empty)) { (x, acc) => Cons(() => f(x, acc.headOption.get), () => acc) }
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    (n, this) match {
      case (_, Empty) | (0, _) => Empty
      case (n, Cons(h, t)) => Cons(h, () => t().take(n - 1))
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (_, 0) | (Empty, _) => None
      case (Cons(h, t), n) => Some(h(), (t(), n - 1))
    }

  def drop(n: Int): Stream[A] =
    (n, this) match {
      case (_, Empty) => Empty
      case (0, _) => this
      case (n, Cons(_, t)) => t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) => {
      if (p(a))
        Cons(() => a, () => b)
      else
        Empty
    }}

  def takeWhile2(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }


  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => p(a) && b }

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

  def headOption2: Option[A] =
    foldRight(None: Option[A]) { (a, _) => Some(a) }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B]) { (a, b) => cons(f(a), b) }

  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A]) { (a, b) => if (f(a)) cons(a, b) else b }

  def append[B >: A](x: => Stream[B]): Stream[B] =
    foldRight(x) { (a, b) => cons(a, b) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B]) { (a, b) => f(a).append(b) }

  def zipWith[B >: A, C](other: Stream[B])(f: (B, B) => C): Stream[C] =
    unfold((this, other)) {
      case ((Cons(h1, t1), Cons(h2, t2))) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    !this.zipWith(s)(_ == _).exists(_ == false)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    }.append(Empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_.startsWith(s))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = cons(a, f(b, a + b))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def fibs2: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def from2(n: Int): Stream[Int] =
    unfold(n) { n => Some(n, n + 1)}

  def constant2[A](a: A): Stream[A] =
    unfold(a) { _ => Some(a, a) }

  def ones2: Stream[Int] = constant2(1)

  def main(args: Array[String]): Unit = {
    val s = cons(1, cons(2, cons(3, cons(5, cons(2, empty)))))
    println(s.toList)
    println(s.take(2).toList)
    println(s.drop(1).toList)
    println(s.takeWhile(x => x < 3).toList)
    println(s.forAll(_ < 5))
    println(s.forAll(_ < 1))
    println(s.takeWhile(_ < 3).toList)
    println(s.map(_ + 1).toList)
    println(s.filter(_ % 2 == 0).toList)
    println(s.append(cons(1, cons(2, empty))).toList)
    println(s.flatMap(cons(_, cons(42, empty))).toList)

    val ss = cons(1, cons(2, empty))
    println(ss.headOption)
    println(ss.headOption2)

    val sss: Stream[Int] = empty
    println(sss.headOption)
    println(sss.headOption2)
    println(sss.map(_ + 1).toList)
    println(sss.append(cons(1, empty)).toList)

    val os = ones
    println(os.take(5).toList)
    val ts = constant(2)
    println(ts.take(5).toList)
    val zz = from(1)
    println(zz.take(5).toList)

    val fs = fibs
    println(fs.take(7).toList)
    println(fibs2.take(7).toList)

    println(from(1).take(5).toList)
    println(constant(5).take(5).toList)
    println(ones.take(5).toList)

    println(s.toList)
    println(s.map(_ + 1).toList)
    println(s.map2(_ + 1).toList)

    println(s.take(2).toList)
    println(s.take2(2).toList)

    println(s.takeWhile(x => x < 3).toList)
    println(s.takeWhile2(x => x < 3).toList)

    val x = cons(1, cons(2, empty))
    val y = cons(1, cons(1, empty))
    println(x.zipWith(y)(_ + _).toList)
    println(x.zipAll(y).toList)
    val z = cons(1, empty)
    println(x.zipAll(z).toList)

    val str1 = cons(1, cons(2, cons(3, empty)))
    val str2 = cons(1, cons(2, empty))
    println(str1.startsWith(str2))
    println(str1.tails.map(_.toList).toList)
    println(str1.hasSubsequence(str2))
    println(str1.hasSubsequence(empty))
    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}