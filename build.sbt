name := "puzzles"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation")

EclipseKeys.withSource := true

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"      at "http://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= {
  Seq(
    "org.scalatest" %% "scalatest" % "2.0.M6-SNAP5" % "test",
    "org.specs2"        %% "specs2"             % "1.13"           % "test",
    "junit" % "junit" % "4.11" % "test"//,
//    "ch.qos.logback" % "logback-classic" % "1.0.9",
//    "log4j" % "log4j" % "1.2.17"
  )
}

