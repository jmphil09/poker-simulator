name := "poker-simulator"
 
version := "1.0"
 
scalaVersion := "2.11.0"
 
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.9" % "test->default",
)