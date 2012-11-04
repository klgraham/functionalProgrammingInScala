object Week6 {
    def scalarProduct(u: Vector[Double], v: Vector[Double]): Double = {
        // (u zip v).map(xy => xy._1 * xy._2).sum
        // (u zip v).map{ case (x, y) => x * y }.sum
        (for ((x, y) <- u zip v) yield x * y).sum
    }

    def isPrime(n: Int): Boolean = (2 until n) forall (d => n %d != 0)

    val n = 7

    (1 until n) flatMap (i =>
        (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))

    for {
      i <- 1 until n
      j <- 1 until i 
      if isPrime(i + j)
    } yield (i, j)
}