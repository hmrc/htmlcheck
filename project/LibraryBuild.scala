import sbt.Keys._
import sbt._
import uk.gov.hmrc.DefaultBuildSettings._
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning
import scoverage.ScoverageKeys

object LibraryBuild extends Build {

  val appName = "htmlcheck"

  lazy val scoverageSettings = Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*BuildInfo.*",
    ScoverageKeys.coverageMinimum := 50,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    ScoverageKeys.coverageEnabled := false
  )

  lazy val valuetype = Project(appName, file("."))
    .enablePlugins(SbtAutoBuildPlugin, SbtGitVersioning)
    .settings(scalaSettings ++ scoverageSettings: _*)
    .settings(
      targetJvm := "jvm-1.8",
      libraryDependencies ++= LibraryDependencies(),
      resolvers := Seq(
        Resolver.bintrayRepo("hmrc", "releases"), "typesafe-releases" at "http://repo.typesafe.com/typesafe/releases/"
      ),
      crossScalaVersions := Seq("2.11.5")
    )

}

private object LibraryDependencies {

  val compile = Seq(
    "org.jsoup" % "jsoup" % "1.9.2",
    "org.typelevel" %% "cats" % "0.7.2"
  )

  val test = Seq(
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.pegdown" % "pegdown" % "1.4.2" % "test"
  )

  def apply() = compile ++ test
}
