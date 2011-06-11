import sbt._

class s99(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    val snapshots = "snapshots" at "http://scala-tools.org/repo-snapshots"
    val releases = "releases" at "http://scala-tools.org/repo-releases"

    val specs2 = "org.specs2" %% "specs2" % "1.3" withSources()
    val scalaz = "org.specs2" %% "scalaz-core" % "6.0.RC2" withSources()

    def extraFramework = new TestFramework("org.specs2.runner.SpecsFramework")
    override def testFrameworks: Seq[TestFramework] = super.testFrameworks ++ Seq(extraFramework)
}
