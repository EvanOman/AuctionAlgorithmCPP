package com.evan.auctionalgorithm

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable._

class AssignmentProblem(val probSize: Int)
{
	val C: DenseMatrix[Double] = ceil(DenseMatrix.rand(this.probSize, this.probSize) * this.probSize.toDouble)
	val INF = Int.MaxValue
	val INFD = Double.MaxValue
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
			assignment = Array.fill(probSize){INF}
			while (assignment.contains(INF))
			{
				iter += 1
				/*
					Update assignment, price with this nasty match / case statement
				*/
				val resTup = auctionRoundPar(assignment, prices, eps)
				assignment = resTup._1; prices = resTup._2
			}
			eps *= .25
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
		/* Maps an object to the bids for that object (bids are (bidder, bid_amount) pairs) */
		val bidMap = collection.mutable.Map[Int, ArrayBuffer[(Int, Double)]]()
		var i = 0

		/* TODO: This would be parallelizable if I find a way to merge the hash map in the end (cost worth it?) */
		while (i < probSize)
		{
			if (u(i) == INF)
			{
				/*
					Need the best and second best value of each object to this person
					where value is calculated row{j} - prices{j}
				*/
				var optObjVal_i = (-1, -INFD)
				var secOptObjVal_i= (-1, -INFD)
				var j = 0
				while (j < probSize)
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
					j += 1
				}
				/* Computes the highest reasonable bid for the best object for this person */
				val (bidObj_i, bidIncr_i) = optObjVal_i
				val bid_i = (bidIncr_i - secOptObjVal_i._2) + eps
				/* Store the bidding info for future use */
				if (bidMap.contains(bidObj_i))
				{
					bidMap(bidObj_i).append((i, bid_i))
				}
				else
				{
					bidMap(bidObj_i) = ArrayBuffer((i, bid_i))
				}
			}
			i += 1
		}
		/*
			We loop over the objects with a bid, chooses the one with the highest bid
		*/
		for ((j, bids) <- bidMap)
		{
			/* Need to get the highest bid for j */
			val topBid_j = bids.maxBy(_._2)
			/* Find other persons who has object j and make them unassigned */
			u = unAssignJ(u, j)
			/* Make assignment, update price */
			u(topBid_j._1) = j
			v(j) += topBid_j._2
		}
		(u, v)
	}

	/* Single Auction Round */
	def auctionRoundPar(assignment: Array[Int], prices: Array[Double], eps: Double): (Array[Int], Array[Double]) =
	{
		/* Local copies */
		var (u, v) = (assignment, prices)
		/* Maps an object to the bids for that object (bids are (bidder, bid_amount) pairs) */
		val bidMap = collection.mutable.Map[Int, (Int, Double)]()

		/* parallelized loop */
		(0 until probSize).par.map(i =>
		{
			if (u(i) == INF)
			{
				/*
					Need the best and second best value of each object to this person
					where value is calculated row{j} - prices{j}
				*/
				var optObjVal_i = (-1, -INFD)
				var secOptObjVal_i= (-1, -INFD)
				var j = 0
				while (j < probSize)
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
					j += 1
				}
				/* Computes the highest reasonable bid for the best object for this person */
				val (bidObj_i, bidIncr_i) = optObjVal_i
				val bid_i = (bidIncr_i - secOptObjVal_i._2) + eps
				/* Store the bidding info for future use */
				(bidObj_i, i, bid_i)
			}
			else
			{
				(-1, -1, -1d)
			}
		}).foreach(bidObj => {
			val (bidItem, bidder, bid) = bidObj
			if (bidItem != -1)
			{
				val curBid: (Int, Double) = bidMap.getOrElse(bidItem, (-1, -1d))
				if (curBid != (-1, -1d))
				{
					if (curBid._2 < bid)
					{
						bidMap(bidItem) = (bidder, bid)
					}
				}
				else
				{
					bidMap(bidItem) = (bidder, bid)
				}
			}
		})
		/*
			We loop over the objects with a bid, chooses the one with the highest bid
		*/
		for ((j, bid) <- bidMap)
		{
			/* Find other persons who has object j and make them unassigned */
			u = unAssignJ(u, j)
			/* Make assignment, update price */
			u(bid._1) = j
			v(j) += bid._2
		}
		(u, v)
	}

	/* Finds the object assigned to j, unassigns it */
	def unAssignJ(arr: Array[Int], j: Int): Array[Int] =
	{
		val local = arr
		var (i, found) = (0, false)
		while (i < probSize && !found)
		{
			if (local(i) == j)
			{
				local(i) = INF
				found = true
			}
			i += 1
		}
		local
	}
}