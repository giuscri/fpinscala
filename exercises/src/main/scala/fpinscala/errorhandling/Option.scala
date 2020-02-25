package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case None=> default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(x => if (f(x)) Some(x) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (_, None) | (None, _) => None
      case (Some(sa), Some(sb)) => Some(f(sa, sb))
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldLeft(Some(Nil): Option[List[A]]) { (acc, x) => (acc, x) match {
      case (None, _) | (_, None) => None
      case (Some(sacc), Some(sx)) => Some(sacc :+ sx)
    }}

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft(Some(Nil): Option[List[B]]) { (acc, x) => (acc, f(x)) match {
      case (None, _) | (_, None) => None
      case (Some(sacc), Some(sfx)) => Some(sacc :+ sfx)
    }}

  def main(args: Array[String]): Unit = {
    val x: Option[Int] = None
    println(x.getOrElse(42))
    println(x.map(Some(_)))
    println(x.filter(_ < 4))

    val y = Some(42)
    println(y.filter(_ < 10))
    println(y.filter(_ > 40))

    println(sequence(List(Some(42), Some(3), Some(4))))
    println(sequence(List(Some(42), Some(3), Some(4), None, Some(5))))
    println(traverse(List(42, 5)) { x => if (x < 100) Some(x) else None})
  }
}