FMPublic

name := "fm-http"

version := "0.2.0-SNAPSHOT"

description := "Async Http Client & Server for Scala"

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4", "2.11.2")

scalacOptions := Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature", "-Xlint", "-optimise")

libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "scala-optparse" % "1.1.1",
  "com.frugalmechanic" %% "fm-common" % "0.2.0-SNAPSHOT",
  "com.frugalmechanic" %% "fm-lazyseq" % "0.1" % "test"
)

libraryDependencies ++= Seq(
  "io.netty" % "netty-all" % "4.0.23.Final",
  "com.jcraft" % "jzlib" % "1.1.3", // For Netty 4.X
  "com.github.jnr" % "jnr-posix" % "3.0.1",             // POSIX Support (getpid and setssid) for the HttpServerApp
  "joda-time" % "joda-time" % "2.3",
  "org.joda" % "joda-convert" % "1.5",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "javax.mail" % "mail" % "1.4.1",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)
