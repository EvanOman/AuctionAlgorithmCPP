package com.evan.auctionalgorithm

object Driver
{
	def main(args: Array[String]): Unit =
	{
		println("Hello World!")
		val name = scala.io.StdIn.readLine("What is your name?\n")
		println(s"Hello $name!!")

		var dict = collection.mutable.Map[String, Int]()
		dict("a") = 1
		dict("b") = 2

		for ((k,v) <- dict)
		{
			println(s"($k, $v)")
		}
	}
}