sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Fold the tree using a right associative binary operator
  def foldRight[A, B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l, foldRight(r, z)(f))(f)
  }

  // Fold the tree using a left associative binary operator
  def foldLeft[A, B](t: Tree[A], z: B)(f: (B, A) => B): B = t match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r, foldLeft(l, z)(f))(f)
  }

  // Returns the number of elements in the tree
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Returns the maximum value of the tree
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Returns the minimum value of the tree
  def minimum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => minimum(l) min minimum(r)
  }

  // Returns the depth of the tree
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Applies a function to each element in the tree and returns a new tree
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Applies a function to each element in the tree and flattens the result
  def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
  }

  // Search for an element in the tree that satisfies a predicate
  def search[A](t: Tree[A], p: A => Boolean): Option[A] = t match {
    case Leaf(a) if p(a) => Some(a)
    case Leaf(_) => None
    case Branch(l, r) => search(l, p).orElse(search(r, p))
  }

  // Check if the tree is a leaf and return the value if it is
  def isLeaf[A](t: Tree[A]): Option[A] = t match {
    case Leaf(a) => Some(a)
    case _ => None
  }
}
