class Newton {
	def abs(x: Double) = if (x < 0) -x else x

	def sqrt(x: Double) = {
		def sqrtIter(guess: Double): Double = 
			if (isGoodEnough(guess)) guess else sqrtIter(improve(guess))

		def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 1.0e-9

		def improve(guess: Double) = (guess + x / guess) / 2

		sqrtIter(1.0)
	}
}