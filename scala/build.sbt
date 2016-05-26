// http://mvnrepository.com/artifact/org.scalanlp/breeze_2.11

name := "Auction Algorithm"

scalaVersion := "2.11.7"


libraryDependencies += "org.scalanlp" % "breeze_2.11" % "0.12"

libraryDependencies += "com.quantifind" %% "wisp" % "0.0.4"

connectInput in run := true
