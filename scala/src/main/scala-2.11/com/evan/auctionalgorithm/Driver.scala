package com.evan.auctionalgorithm

import breeze.linalg._
import breeze.numerics._

object Driver
{
	def main(args: Array[String]): Unit =
	{
		val probSize = scala.io.StdIn.readLine("Please enter a problem size: ").toInt
		auction(probSize)
	}

	/* Main Auction Loop */
	def auction(size: Int): Unit =
	{
		val C: DenseMatrix[Double] = ceil(DenseMatrix.rand(size, size) * size.toDouble)
	}

	/* Single Auction Round */
	def auctionRound(assignment: Vector[Int], prices: Vector[Double], C: DenseMatrix[Double],
					 eps: Double): (Vector[Int], Vector[Double]) =
	/* TODO: No need for C to be passed around, created problem class with access to problem specific consts */
	{
		(new DenseVector(Array[Int]()), new DenseVector[Double](Array[Double]()))
	}
}