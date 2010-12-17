import sbt._

class MyProject(info: ProjectInfo) extends DefaultProject(info) with Exec {
  	val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
  	val scalaSnapshots = "Scala Snapshots repo" at "http://scala-tools.org/repo-snapshots"	

	val junit = "junit" % "junit" % "4.7" % "test" withSources
	val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7-SNAPSHOT" % "test" withSources
	val mockito = "org.mockito" % "mockito-all" % "1.8.4" % "test" withSources
}
