name := "scala-fun"

version := "0.1"

scalaVersion := "2.13.1"
//lazy val commonSettings = Seq(
//  //  organization := "com.actionml",
//  //  scalaVersion := "2.10.7",
//  updateOptions := updateOptions.value.withLatestSnapshots(false),
//  resolvers += Resolver.bintrayRepo("hseeberger", "maven"),
//  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
//  resolvers += Resolver.mavenLocal,
//  // 将阿里云仓库做为默认仓库
//  externalResolvers := List("my repositories" at "https://maven.aliyun.com/nexus/content/groups/public/"),
//  libraryDependencies ++= Seq(
//    "org.scalactic" %% "scalactic" % "3.1.1",
//    "org.scalatest" %% "scalatest" % "3.1.1" % "test"
//  ),
//)
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.example"

resolvers := Seq(
//  Resolver.url("aliyun", url("https://maven.aliyun.com/nexus/content/groups/public/")),
  Resolver.mavenLocal,
  DefaultMavenRepository,
)

lazy val hello = (project in file("."))
  .settings(
    name := "Hello",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.1.1",
      "org.scalatest" %% "scalatest" % "3.1.1" % "test"
    )
  )

