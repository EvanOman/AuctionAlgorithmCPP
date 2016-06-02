package com.evan.auctionalgorithm

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable._

class AssignmentProblem(val probSize: Int)
{
	val C: DenseMatrix[Double] = ceil(DenseMatrix.rand(this.probSize, this.probSize) * this.probSize.toDouble)
	val INF = Int.MaxValue
	val VERBOSE = false

	/* Main Auction Loop */
	def runAuction(): Unit =
	{
		var assignment = Array.fill(probSize){INF}
		var prices = Array.fill(probSize){1d}
		var eps = 1d
		var iter = 1

		val beginT = System.nanoTime()
		while (eps > 1d / probSize)
		{
			/* Reset the assignment vector, only the prices persist */
			assignment = assignment.map(x => INF)
			while (assignment.contains(INF))
			{
				iter += 1
				/*
					Update assignment, price with this nasty match / case statement
				*/
				val resTup = auctionRound(assignment, prices, eps)
				assignment = resTup._1; prices = resTup._2
			}
			eps *= .25
		}
		if (VERBOSE)
		{
			println("Final Assignments: ")
			assignment.zipWithIndex.foreach(x => println(s"$x"))
		}

		/* Calculate time difference */
		val diff = (System.nanoTime() - beginT) / 1e9d
		println(s"Took $diff seconds")
	}

	/*
		TODO: This whole auctionRound routine reeks of mutability, is there a more functional approach?
	*/

	/* Single Auction Round */
	def auctionRound(assignment: Array[Int], prices: Array[Double], eps: Double): (Array[Int], Array[Double]) =
	{
		/* Local copies */
		var (u, v) = (assignment, prices)
		/* Array of tuples: bidder, item, bid*/
		val bids = ArrayBuffer.empty[(Int, Int, Double)]
		for (i <- 0 until probSize)
		{
			if (u(i) == INF)
			{
				/*
					Need the best and second best value of each object to this person
					where value is calculated row_{j} - prices{j}
				*/
				var optObjVal_i = (-1, -INF.toDouble)
				var secOptObjVal_i= (-1, -INF.toDouble)
				for (j <- 0 until probSize)
				{
					val curVal = C(i, j) - v(j)
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
				val bid_i = (optObjVal_i._2 - secOptObjVal_i._2) + eps
				/* Store the bidding info for future use */
				bids.append((i, optObjVal_i._1, bid_i))
			}
		}
		if (VERBOSE) bids.foreach(x => println(s"$x"))
		/*
			We loop over the objects with a bid, chooses the one with the highest bid
		*/
		for (j <- 0 until probSize)
		{
			/* Find objects which have selected j */
			val want_j = bids.filter(_._2 == j)
			if (want_j.nonEmpty)
			{
				/* Need to get the highest bid for j */
				val topBid_j = want_j.reduce((x, y) => if (x._3 > y._3) x else y)
				/* Find other persons who has object j and make them unassigned */
				u = u.zipWithIndex.map(x => {
					if (x._1 == j)
					{
						if (VERBOSE) println(s"Unassigning pair $x")
						INF
					}
					else
						x._1
				})
				/* Make assignment, update price */
				if (VERBOSE) println(s"Making assignment $topBid_j")
				u(topBid_j._1) = j
				v(j) += topBid_j._3
			}
		}
		(u, v)
	}
}