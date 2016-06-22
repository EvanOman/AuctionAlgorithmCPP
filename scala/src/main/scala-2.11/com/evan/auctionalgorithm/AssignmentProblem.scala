package com.evan.auctionalgorithm

import java.io.File

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable._

/*
	Creates a problem instance where we are trying to assign people to objects
*/
class AssignmentProblem(val C: DenseMatrix[Int])
{
	val nPers = C.rows
	val nObjs = C.cols
	val INF = Int.MaxValue
	val INFD = Double.MaxValue
	val VERBOSE = false

	/* Random n*m problem instance*/
	def this(n: Int, m: Int) = this(util.randIntMat(n, m))

	/* Random square problem instance*/
	def this(probSize: Int) = this(probSize, probSize)

//	/* Reference problem instance */
//	def this() = this(csvread(new File("./dat/C.csv")))

	/* Main Auction Loop */
	def runAuction(roundFun: (Array[Int], Array[Double], Double) => (Array[Int], Array[Double])): (Matrix[Int], Int) =
	{
		var assignment = Array.fill(nPers){INF}
		var prices = Array.fill(nObjs){1d}
		var eps = 1d
		var iter = 1
		val beginT = System.nanoTime()
		while (eps > 1d / nPers)
		{
			/* Reset the assignment vector, only the prices persist */
			assignment = Array.fill(nPers){INF}
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
		val formatter = java.text.NumberFormat.getNumberInstance
		val score = scoreAssig(assignment)
		val sc = formatter.format(score)
//		println(s"Auction complete, best score: $sc")

		(assigArr2Mat(assignment), score)
	}

	/* Default to sequential implementation */
	def runAuction(): (Matrix[Int], Int) =
	{
		runAuction(auctionRound)
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

		while (i < nPers)
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
				while (j < nObjs)
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
		(0 until nPers).filter(u(_) == INF).par.map(i =>
		{
			/*
				Need the best and second best value of each object to this person
				where value is calculated row{j} - prices{j}
			*/
			var optObjVal_i = (-1, -INFD)
			var secOptObjVal_i= (-1, -INFD)
			var j = 0
			while (j < nObjs)
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
		while (i < nPers && !found)
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

	def assigArr2Mat(assigVec: Array[Int]): Matrix[Int] =
	{
		Matrix.tabulate[Int](nPers, nObjs){case (i, j) => if (assigVec(i) == j) 1 else 0}
	}

	def runEnum(): (Matrix[Int], Double) =
	{
//		println("Running enum...")
		/*
			Loop over all possible assignments, finding the best
			The way this works is I want all permutations of all combinations of nObjs persons
			The order of each permutation then defines the assignment
		*/
		var best = (0 until nPers).toArray
		var bestScore = scoreAssig(best)
		var counter = 0
		(0 until nObjs).combinations(nPers).map(_.permutations).reduce(_++_).foreach(
			assig =>
			{
				val score = scoreAssig(assig.toArray)
				if (score > bestScore)
				{
					best = assig.toArray
					bestScore = score
				}
				counter = counter + 1
			}
		)
		val formatter = java.text.NumberFormat.getNumberInstance
		val sc = formatter.format(bestScore)
//		println(s"Checked $counter permuations")
//		println(s"Enum complete, best score: $sc")
		(assigArr2Mat(best), bestScore)
	}

	/* Score the assignment vector: List[(obj, pers)]*/
	def scoreAssig(assig: Array[Int]): Int =
	{
		assig.zipWithIndex.foldLeft(0)((acc, i) => acc + C(i._2, i._1))
	}
}

object util
{
	val r = scala.util.Random
	def randIntMat(n: Int, m: Int): DenseMatrix[Int] = DenseMatrix.tabulate[Int](n,m){case _ => r.nextInt(n*m)}
}