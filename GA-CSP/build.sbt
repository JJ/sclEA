
name := "GA-CSP"

version := "0.0.1"

scalaVersion := "2.11.6"

fork := true

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

testOptions in Test += Tests.Argument("-oDF")
//testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")

scalacOptions := Seq("-deprecation", "-feature")