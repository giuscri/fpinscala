package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) { (x,xs) => Cons(x, xs) }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def sum4(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]) =
    foldRight(ns, 1.0) { (x,y) => x match {
      case 0 => x
      case x => x * y
    }}

  def product4(ns: List[Double]) =
    foldLeft(ns, 0.0)((x,y) => x * y)

  def length4[A](ns: List[A]): Int =
    foldLeft(ns, 0)((x,_) => 1 + x)

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case _ => Cons(h, Nil)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) => drop(t, n - 1)
    case _ => Nil
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case _ => Nil
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0) { (x,y) => 1 + y }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A]) { (z, x) => Cons(x, z) }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    val ll = reverse(l)
    foldRight(ll, z) { (x,y) => f(y,x) }
  }

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    val ll = reverse(l)
    foldLeft(ll, z) { (x,y) => f(y,x) }
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def listOfListsIntoSingleList[A](ll: List[List[A]]): List[A] = {
    foldLeft(ll, Nil: List[A]) { (xs, ys) => append(xs, ys) }
  }

  def main(args: Array[String]): Unit = {
    println(init(List(1, 2, 3, 4)))
    println(dropWhile(List(1, 2, 3, 4, 5)) { x => x < 2 })
    println(product3(List(1, 2, 0, 2, 3)))
    println(foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
    println(length(List(1, 2, 3, 4, 5, 6)))
    println(foldLeft(List(1, 2, 3), 0)((x,y) => x + y))
    println(sum4(List(1, 2, 3)))
    println(product4(List(1, 2, 3)))
    println(length4(List(1, 2, 3, 4, 5, 6)))
    println(reverse(List(1, 2, 3)))
    println(foldRight2(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
    println(foldLeft2(List(1, 2, 3), 0)((x,y) => x + y))
    println(append2(List(1,2,3), List(4,5,6)))
    println(listOfListsIntoSingleList(List(List(1,2,3), List(4,5,6), List(7,8,))))
  }
}
