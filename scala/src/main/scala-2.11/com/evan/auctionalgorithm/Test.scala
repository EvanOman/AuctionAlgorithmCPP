package com.evan.auctionalgorithm

import breeze.linalg.{DenseMatrix, all}


object Test
{
	def main(args: Array[String]): Unit =
	{
		var i = 0
		val n = 100
		val percent = (0 until n).map(i =>
		{
			val testC = -1 * util.randIntMat(8,8)
			val auc = new AssignmentProblem(testC)
			val (u1: DenseMatrix[Int], s1: Int) = auc.auctionSolve(auc.auctionRound)
			val (u2: DenseMatrix[Int], s2: Int) = auc.enumSolve()
			if (s2 == s1) 1 else {println((s1, s2)); println(u1); println(); println(u2); 0}
		}).seq.sum / n.toDouble

		val formatter = java.text.NumberFormat.getNumberInstance
		val p = formatter.format(percent)
		println(s"Percentage correct: $p")
	}
}