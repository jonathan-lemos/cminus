name := "cminus"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

scalacOptions ++= Seq(
	"-target:jvm-1.8",
	"-encoding", "UTF-8",
	"-unchecked",
	"-deprecation",
	"-Xfuture",
	"-Yno-adapted-args",
	"-Ywarn-dead-code",
	"-Ywarn-numeric-widen",
	"-Ywarn-value-discard",
	"-Ywarn-unused",
	"-feature",
	"-language:existentials",
	"-language:higherKinds",
	"-language:implicitConversions"
)