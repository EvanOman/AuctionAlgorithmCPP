package com.evan.auctionalgorithm


object Driver
{
	def main(args: Array[String]): Unit =
	{
		runTiming()
	}

	def runTiming(): Unit =
	{
		val runTimes = (100 to 7500 by 200).map{ i => {
			val (avg1, avg2) = (0 until 5).map{_ => {
				val auc = new AssignmentProblem(i)
				val t1 = auc.runAuction(auc.auctionRound)
				val t2 = auc.runAuction(auc.auctionRoundPar)
				(t1/5d, t2/5d)
			}}.toList.reduce{(x,y) => (x._1 + y._1, x._2 + y._2)}
			(i, avg1, avg2)
		}
		}.toList

		val (sizes, seq, par) = runTimes.unzip3
		println("Sequential times:")
		println(sizes.zip(seq))

		println("Parallel times:")
		println(sizes.zip(par))
	}
}