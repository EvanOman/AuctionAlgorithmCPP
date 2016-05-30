package com.evan.auctionalgorithm

import breeze.linalg._
import breeze.numerics._

class AssignmentProblem(val probSize: Int)
{
	val C: DenseMatrix[Double] = ceil(DenseMatrix.rand(this.probSize, this.probSize) * this.probSize.toDouble)
	val INF = Int.MaxValue

	/* Main Auction Loop */
	def runAuction(): Unit =
	{
		var assignment = Vector.fill(probSize){INF}
		var prices = Vector.fill(probSize){1d}
		var eps = 1d
		var iter = 1

		while (eps > 1 / probSize)
		{
                        assignment = assignment.map(x => INF)
                        
                        while (assignment.filter(_ != INF).size > 0)
                        {
                            iter += 1
                            assignment = assignment.map(x => 0)
                        }
			eps = 0 // Exit for now
		}
	}

	/* Single Auction Round */
	def auctionRound(assignment: Vector[Int], prices: Vector[Double], eps: Double): (Vector[Int], Vector[Double]) =
	{
		(new DenseVector(Array[Int]()), new DenseVector[Double](Array[Double]()))
	}

}
