package com.evan.auctionalgorithm


object Driver
{
	def main(args: Array[String]): Unit =
	{
		val probSize = 10000//scala.io.StdIn.readLine("Please enter a problem size: ").toInt
		val myAuction = new AssignmentProblem(probSize)
		myAuction.runAuction()
	}
}