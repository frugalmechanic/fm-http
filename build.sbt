FMPublic

name := "fm-http"

version := "0.4.0-SNAPSHOT"

description := "Async Http Client & Server for Scala"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7")

scalacOptions := Seq("-unchecked", "-deprecation", "-language:implicitConversions", "-feature", "-Xlint", "-optimise")

libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "scala-optparse" % "1.1.2",
  "com.frugalmechanic" %% "fm-common" % "0.4.0-SNAPSHOT",
  "com.frugalmechanic" %% "fm-lazyseq" % "0.3.0" % "test"
)

libraryDependencies ++= Seq(
  "io.netty" % "netty-all" % "4.0.33.Final",
  "com.jcraft" % "jzlib" % "1.1.3", // For Netty 4.X
  "com.github.jnr" % "jnr-posix" % "3.0.1", // POSIX Support (getpid and setssid) for the HttpServerApp
  "joda-time" % "joda-time" % "2.9.1",
  "org.joda" % "joda-convert" % "1.8", // Required by joda-time when using Scala
  "org.slf4j" % "slf4j-api" % "1.7.13",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "javax.mail" % "mail" % "1.4.1",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
