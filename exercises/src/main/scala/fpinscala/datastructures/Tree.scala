package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def size2[A](tree: Tree[A]): Int =
    fold(tree) { _ => 1 } { (fl, fr) => {
      1 + fl + fr
    }}

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

  def maximum2(tree: Tree[Int]): Int =
    fold(tree) { x => x } { (fl, fr) => {
      fl.max(fr)
    }}

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

  def depth2[A](tree: Tree[A]): Int =
    fold(tree) { _ => 0 } { (fl, fr) => {
      1 + fl.max(fr)
    }}

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree) { x => Leaf(f(x)) } { (fl, fr) => {
      Branch(fl, fr)
    }}

  def fold[A, B](tree: Tree[A])(l: A => B)(b: (B,B) => B): B =
    tree match {
      case Leaf(x) => l(x)
      case Branch(lb, rb) => b(fold(lb)(l)(b), fold(rb)(l)(b))
    }

  def main(args: Array[String]): Unit = {
    var tree: Tree[Int] = Leaf(1)
    println(size(tree))
    println(size2(tree))

    tree = Branch(Leaf(1), Leaf(3))
    println(size(tree))
    println(size2(tree))

    tree = Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(4), Leaf(6)))
    println(size(tree))
    println(size2(tree))
    println(maximum(tree))
    println(maximum2(tree))
    println(depth(tree))
    println(depth2(tree))
    println(map(tree) { x => x.toDouble })
    println(map2(tree) { x => x.toDouble })
  }

}