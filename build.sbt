name := """million"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  "com.typesafe.slick" %% "slick"                % "2.1.0",
  // "com.typesafe.play"  %% "play-slick"           % "0.8.1",
  "mysql"              %  "mysql-connector-java" % "5.1.20",
  "org.scalaz"         %% "scalaz-core"          % "7.1.4"
)
