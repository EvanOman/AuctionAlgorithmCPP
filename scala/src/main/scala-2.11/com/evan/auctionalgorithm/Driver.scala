package com.evan.auctionalgorithm

import breeze.linalg.all


object Driver
{
	def main(args: Array[String]): Unit =
	{
		val auc = new AssignmentProblem()
		val (u1: Array[Int], t1) = auc.runAuction(auc.auctionRound)
		val (u2: Array[Int], t2) = auc.runAuction(auc.auctionRoundPar)
		val good = u1.zip(u2).map{x => x._1 == x._2}.reduce(_ && _)
		val (u1S, u2S) = (u1.mkString("\t"), u2.mkString("\t"))
		val formatter = java.text.NumberFormat.getNumberInstance
		val (t1S, t2S) = (formatter.format(t1), formatter.format(t2))
		println(s"Assignments are identical: $good")
		println(s"Seq took $t1S seconds, assignment:\t $u1S")
		println(s"Par took $t2S seconds, assignment:\t $u2S")
//		runTiming()
	}



//	def runTiming(): Unit =
//	{
//		val runTimes = (100 to 7500 by 200).map{ i => {
//			val (avg1, avg2) = (0 until 5).map{_ => {
//				val auc = new AssignmentProblem()
//				val t1 = auc.runAuction(auc.auctionRound)
//				val t2 = auc.runAuction(auc.auctionRoundPar)
//				(t1/5d, t2/5d)
//			}}.toList.reduce{(x,y) => (x._1 + y._1, x._2 + y._2)}
//			(i, avg1, avg2)
//		}}.toList
//
//		val (sizes, seq, par) = runTimes.unzip3
//		println("Sequential times:")
//		println(sizes.zip(seq))
//
//		println("Parallel times:")
//		println(sizes.zip(par))
//	}
}