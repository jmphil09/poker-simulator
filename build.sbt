name := "poker-simulator"
 
version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.3.0",
  "org.webjars" % "bootstrap" % "2.3.1",
  "org.webjars" % "requirejs" % "2.1.11-1",
  //"org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
  "org.scalatestplus" % "play_2.11" % "1.1.0" % "test",
  "junit" % "junit" % "4.11" % "test",
  "com.novocode" % "junit-interface" % "0.9" % "test->default"
)

lazy val root = (project in file(".")).addPlugins(PlayScala)
