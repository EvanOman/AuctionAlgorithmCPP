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
		var assignment = Array.fill(probSize){INF}
		var prices = Array.fill(probSize){1d}
		var eps = 1d
		var iter = 1

		while (eps > 1 / probSize)
		{
			/* Reset the assignment vector, only the prices persist */
			assignment = assignment.map(x => INF)
			while (assignment.contains(INF))
			{
				iter += 1
				/*
					Update assignment, price with this nasty match / case statement
					(no tuple pattern matching for existing vars)
				*/
				val resTup = auctionRound(assignment, prices, eps)
				assignment = resTup._1; prices = resTup._2
			}
			eps *= .25

			eps = 0 // Exit for now
		}
	}

	/*
		TODO: This whole auctionRound routine reeks of mutability, is there a more functional approach?
	*/

	/* Single Auction Round */
	def auctionRound(assignment: Array[Int], prices: Array[Double], eps: Double): (Array[Int], Array[Double]) =
	{
		/*
			These are meant to be kept in correspondence such that bidded[i]
			and bids[i] correspond to person i bidding for bidded[i] with bid bids[i]
		*/
		var (tmpBidded, tmpBids, unAssig) = (Array[Int](), Array[Double](), Array.fill(probSize){false})

		for (i <- 0 to probSize)
		{
			if (assignment(i) == INF)
			{
				/* Add i to the unassigned list */
				unAssig(i) = true
				/*
					Need the best and second best value of each object to this person
					where value is calculated row_{j} - prices{j}
				*/
				var optObjVal_i = (-1, -1.toDouble)
				var secOptObjVal_i= (-INF, -INF.toDouble)
				for (j <- 0 to probSize)
				{
					val curVal = C(i, j)
					if (curVal > optObjVal_i._2)
					{
						/* Update book keeping, assign new best val/obj */
						secOptObjVal_i = optObjVal_i
						optObjVal_i = (j, curVal)
					}
					else if (curVal > secOptObjVal_i._2)
					{
						secOptObjVal_i = (j, curVal)
					}
				}

				/* Computes the highest reasonable bid for the best object for this person */
			}
		}

		(new Array[Int](1), new Array[Double](1))
	}
}