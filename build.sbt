//import play.Project._

name := "poker-simulator"
 
version := "1.0"

//scalaVersion := "2.10.0"
scalaVersion := "2.11.0"
 
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.9" % "test->default"
)