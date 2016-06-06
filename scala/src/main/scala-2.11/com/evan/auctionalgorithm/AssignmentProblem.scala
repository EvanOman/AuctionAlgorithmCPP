package com.evan.auctionalgorithm

import java.io.File

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable._

class AssignmentProblem(val C: DenseMatrix[Double])
{
	val probSize = C.cols
	val INF = Int.MaxValue
	val INFD = Double.MaxValue
	val VERBOSE = false

	/* Random problem instance*/
	def this(probSize: Int) = this(ceil(DenseMatrix.rand(probSize, probSize) * probSize.toDouble))

	/* Reference problem instance */
	def this() = this(csvread(new File("./dat/C.csv")))

	/* Main Auction Loop */
	def runAuction(roundFun: (Array[Int], Array[Double], Double) => (Array[Int], Array[Double])): (Array[Int], Double) =
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
				val resTup = roundFun(assignment, prices, eps)
				assignment = resTup._1; prices = resTup._2
			}
			eps *= .25
		}
		/* Calculate time difference */
		val diff: Double = (System.nanoTime() - beginT) / 1e9d
		(assignment, diff)
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
		val bidMap = collection.mutable.Map[Int, (Int, Double)]()
		var i = 0

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
				val (bidItem, bidIncr_i) = optObjVal_i
				val bid_i = (bidIncr_i - secOptObjVal_i._2) + eps
				/* Keep the best bid */
				val curBid: (Int, Double) = bidMap.getOrElse(bidItem, (-1, -1d))
				if (curBid != (-1, -1d))
				{
					if (curBid._2 < bid_i)
					{
						bidMap(bidItem) = (i, bid_i)
					}
				}
				else
				{
					bidMap(bidItem) = (i, bid_i)
				}
			}
			i += 1
		}
		/*
			We loop over the objects with a bid, chooses the one with the highest bid
		*/
		for ((j, bidObj) <- bidMap)
		{
			/* Need to get the highest bid for j */
			val (bidder, bid) = bidObj
			/* Find other persons who has object j and make them unassigned */
			u = unAssignJ(u, j)
			/* Make assignment, update price */
			u(bidder) = j
			v(j) += bid
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
		(0 until probSize).filter(u(_) == INF).par.map(i =>
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
		}).toList.foreach(bidObj => {
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