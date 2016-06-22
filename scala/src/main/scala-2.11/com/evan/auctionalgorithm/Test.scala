package com.evan.auctionalgorithm

import breeze.linalg.{DenseMatrix, all}


object Test
{
	def main(args: Array[String]): Unit =
	{
		var i = 0
//		val percent = (0 to 2).par.map(i =>
//		{
			val testC = util.randIntMat(10,8)
			val auc = new AssignmentProblem(testC)
			val (u1: DenseMatrix[Int], s1) = auc.runAuction(auc.auctionRound)
//			val (u2: DenseMatrix[Int], _) = auc.runAuction(auc.auctionRoundPar)
			val (u2: DenseMatrix[Int], s2) = auc.runEnum()
			println((s1, s2))
			if (s2 == s1) 1 else 0
			//		val (u1S, u2S) = (u1.mkString("\t"), u2.mkString("\t"))
//			val formatter = java.text.NumberFormat.getNumberInstance
//			val (t1S, t2S) = (formatter.format(t1), formatter.format(t2))
//			println(s"Assignments are identical: $good")
//		println(s"Seq took $t1S seconds, assignment:\n$u1")
//		println(s"Par took $t2S seconds, assignment:\n$u2")
//		println(s"Here is the enum assignment:\n$u3")
//		}).seq.sum / 100d

//		val formatter = java.text.NumberFormat.getNumberInstance
//		val p = formatter.format(percent)
//		println(s"Percentage correct: $p")
	}
}