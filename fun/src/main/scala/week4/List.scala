// package week4

// trait List[T] {
//   def isEmpty: Boolean
//   def head: T
//   def tail: List[T]
// }

// class Cons[T](val head: T, val tail: List[T]) extends List[T] {
//   def isEmpty = false
// }

// class Nil[T] extends List[T] {
//   def isEmpty = true
//   def head: Nothing = throw new NoSuchElementException("Nil.head")
//   def tail: Nothing = throw new NoSuchElementException("Nil.tail")
// }

// // object Week4 {
// //   def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

// //  }

// object List {
//   def apply[T]: List[T] = new Nil
//   def apply[T](n: T): List[T] = new Cons(n, new Nil)
//   def apply[T](m: T, n: T): List[T] = new Cons(m, new Cons(n, new Nil))
// }