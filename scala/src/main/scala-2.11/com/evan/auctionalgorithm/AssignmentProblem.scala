package com.evan.auctionalgorithm

import breeze.linalg._
import breeze.numerics._

class AssignmentProblem(val problemSize: Int)
{
	val C: DenseMatrix[Double] = ceil(DenseMatrix.rand(this.problemSize, this.problemSize) * this.problemSize.toDouble)
	val INF = Int.MaxValue

	/* Main Auction Loop */
	def runAuction(): Unit =
	{
		var assignment = Vector.fill(problemSize){INF}
		var prices = Vector.fill(prices){1d}
		var eps = 1d
		var iter = 1

		while (eps > 1 / problemSize)
		{
			iter += 1
			eps = 0 // Exit for now
		}
	}

	/* Single Auction Round */
	def auctionRound(assignment: Vector[Int], prices: Vector[Double], eps: Double): (Vector[Int], Vector[Double]) =
	{
		(new DenseVector(Array[Int]()), new DenseVector[Double](Array[Double]()))
	}

}
