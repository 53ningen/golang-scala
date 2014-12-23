name := "golang"

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.15" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
)
