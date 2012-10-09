object Week2 {
	
	// tail recursive factorial function
	// this is a first-class function, so it operates on data types
	def factorial(n: Int): Int  = {
		def iteration(product: Int, iter: Int): Int = {
			if (iter > n) product 
			else iteration(product * iter, iter + 1)
		}
		iteration(1, 1)
	}

	/*
	* higher-order functions take other functions as parameters. 
	* here are some examples
	*/

	// first-class ways to do \Sum_{x=a}^b f(x)

	// f(x) = x
	def sumInts(a: Int, b: Int): Int = {
		if (a > b) 0 
		else a + sumInts(a + 1, b)
	}

	// f(x) = x^3
	def cube(a: Int): Int = a * a * a
	def sumCubes(a: Int, b: Int): Int = {
		if (a > b) 0 
		else cube(a) + sumCubes(a + 1, b)
	}

	// f(x) = x!
	def sumFactorials(a: Int, b: Int): Int = {
		if (a > b) 0 
		else factorial(a) + sumFactorials(a + 1, b)
	}

	// the same as higher-order functions
	def sum(f: Int => Int, a: Int, b: Int): Int = {
		if (a > b) 0
		else f(a) + sum(f, a + 1, b)
	}

	def id(a: Int): Int = a

	/* In Scala, functions are maps or morphisms from one category to another category
	* For example, the functions used above are all morphisms from the category 
	* of integers to the category of integers
	*/

	// tail-recursive sum function
	def sum_tr(f: Int => Int)(a: Int, b: Int): Int = {
		def loop(a:Int, acc: Int): Int = {
			if (a > b) acc
			else loop(a + 1, acc + f(a)) 
		}
		loop(a, 0)
	}

	// currying

	/*

	*/
	def sum_curry(f: Int => Int):  (Int,  Int) => Int = {
		def sumF(a: Int, b: Int): Int = 
			if (a > b) 0
			else f(a) + sumF(a + 1, b)
		sumF
	}

	// this is \Product_{n=a}^b f(n)

	def curried_product(f: Int => Int): (Int, Int) => Int = {
		def prodF(a: Int,b: Int): Int = {
			if (a > b) 1
			else f(a) * prodF(a +1, b)
		}
		prodF
	}

	def fact(n: Int): Int = product(x => x)(1, n)

	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
		if (a > b) zero
		else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
	}

	def product(f: Int => Int)(a: Int,b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

	// fixed points
	import math.{abs}

	// f(x) = x
	// iterate f(f(f(f(...(x)))) = x
	val tolerance = 0.0001
	def isCloseEnough(x: Double, y: Double) = 
		abs((x - y) / x) / x < tolerance
	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
		def iterate(guess: Double): Double = {
			// println("guess = " + guess)
			val next = f(guess)
			if (isCloseEnough(guess, next)) next
			else iterate(next)
		}
		iterate(firstGuess)
	}

	/* 
	* apply to sqrt function
	* \sqrt(x) = y means y^2 = x
	* or \sqrt(x) = y means that y = x/ y
	* Thus sqrt(x) isa fixed point of the function y => x/y
	*/

	// def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)
	// we're averaging the current and previous guesses

	def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
	def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)
}