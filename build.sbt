name := "scala-fun"

version := "0.1"

scalaVersion := "2.13.10"

resolvers := Seq(
  Resolver.mavenLocal,
  DefaultMavenRepository,
)

lazy val hello = (project in file("."))
  .settings(
    name := "Hello",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.3.1",
      "org.scalactic" %% "scalactic" % "3.1.1",
      "org.scalatest" %% "scalatest" % "3.1.1" % "test"
    )
  )

