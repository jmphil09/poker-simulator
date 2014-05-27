import sbt._
import Keys._
//import PlayProject._
import play.Project._
 
object ApplicationBuild extends Build {
 
  val appName         = "PlayProject"
  val appVersion      = "1.0"
 
  val appDependencies = Nil
 
  val main = play.Project(
    appName, appVersion, appDependencies//, mainLang = SCALA
  ) 
 
}
