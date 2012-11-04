//Week5.scala

import math.Ordering

object Week5 {
    def main(args: Array[String]) = {
        val nums = List(4,7,3,-2,-9)
        val fruit = List("apple", "orange", "grapres", "bananas")
        mergesort(nums)
        mergesort(fruit)

        val data = List("a", "a", "a", "b", "c", "c", "a")
    }

    def mergesort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
        val n = xs.length / 2
        if (n == 0) xs
        else {
            def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
                case (Nil, ys) => ys
                case (xs, Nil) => xs
                case (x :: xs1, y :: ys1) => 
                  if (ord.lt(x, y)) x :: merge(xs1, ys)
                  else y :: merge(xs, ys1)
            }
            val (a, b) = xs.splitAt(xs.length / 2)
            merge(mergesort(a), mergesort(b))
        }
    }

    def fApplyList(f: Int => Int)(xs: List[Int]): List[Int] = xs match {
        case Nil      => Nil
        case y :: ys => f(y) :: fApplyList(f)(ys)
    }

    def fApplyList2(f: Int => Int)(xs: List[Int]): List[Int] = 
        xs map (x => f(x))

    def pack[T](xs: List[T]): List[List[T]] = xs match {
        case Nil => Nil
        case y :: ys1 =>
            val (first, rest) = xs span (x => x == y)
            first :: pack(rest)
    }
    
    // can use something like this for compression
    def encode[T](xs: List[T]): List[(T, Int)] = xs match {
        case Nil => Nil
        case y :: ys1 =>
            pack(xs) map (ys => (ys.head, ys.length))
    }

    def sum(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
    def prod (xs: List[Int]): Int = (xs foldLeft 1)(_ * _)
}